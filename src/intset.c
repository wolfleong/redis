/*
 * Copyright (c) 2009-2012, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 * Copyright (c) 2009-2012, Salvatore Sanfilippo <antirez at gmail dot com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Redis nor the names of its contributors may be used
 *     to endorse or promote products derived from this software without
 *     specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "intset.h"
#include "zmalloc.h"
#include "endianconv.h"
#include "redisassert.h"

/* Note that these encodings are ordered, so:
 * INTSET_ENC_INT16 < INTSET_ENC_INT32 < INTSET_ENC_INT64. */
//16位编码类型
#define INTSET_ENC_INT16 (sizeof(int16_t))
//32位编码类型
#define INTSET_ENC_INT32 (sizeof(int32_t))
//64位编码类型
#define INTSET_ENC_INT64 (sizeof(int64_t))

//根据给定的int值的大小, 返回最适合的编码类型
/* Return the required encoding for the provided value. */
static uint8_t _intsetValueEncoding(int64_t v) {
    if (v < INT32_MIN || v > INT32_MAX)
        return INTSET_ENC_INT64;
    else if (v < INT16_MIN || v > INT16_MAX)
        return INTSET_ENC_INT32;
    else
        return INTSET_ENC_INT16;
}

//根据编码, 获取集合中给定索引的值返回
/* Return the value at pos, given an encoding. */
static int64_t _intsetGetEncoded(intset *is, int pos, uint8_t enc) {
    int64_t v64;
    int32_t v32;
    int16_t v16;

    //如果是64位编码
    if (enc == INTSET_ENC_INT64) {
        //将数组强转成64位的数组, 并且拷贝给定位置64位的值到 v64 变量中返回
        memcpy(&v64,((int64_t*)is->contents)+pos,sizeof(v64));
        memrev64ifbe(&v64);
        return v64;
        //获取32位编码获取给定索引的值
    } else if (enc == INTSET_ENC_INT32) {
        memcpy(&v32,((int32_t*)is->contents)+pos,sizeof(v32));
        memrev32ifbe(&v32);
        return v32;
    } else {
        //获取16位编码给索引的值
        memcpy(&v16,((int16_t*)is->contents)+pos,sizeof(v16));
        memrev16ifbe(&v16);
        return v16;
    }
}

//获取给定索引的值
/* Return the value at pos, using the configured encoding. */
static int64_t _intsetGet(intset *is, int pos) {
    return _intsetGetEncoded(is,pos,intrev32ifbe(is->encoding));
}

//将值设置到集合的给定索引位置上
/* Set the value at pos, using the configured encoding. */
static void _intsetSet(intset *is, int pos, int64_t value) {
    //获取intSet的编码
    uint32_t encoding = intrev32ifbe(is->encoding);

    //如果是64位, 将数组强转为64位数据, 然后将 int64 的值直接赋值到给定位置上
    if (encoding == INTSET_ENC_INT64) {
        ((int64_t*)is->contents)[pos] = value;
        memrev64ifbe(((int64_t*)is->contents)+pos);
        //如果是32位, 将数组强转为32位数据, 然后将 int32 的值直接赋值到给定位置上
    } else if (encoding == INTSET_ENC_INT32) {
        ((int32_t*)is->contents)[pos] = value;
        memrev32ifbe(((int32_t*)is->contents)+pos);
    } else {
        //如果是16位, 将数组强转为16位数据, 然后将 int16 的值直接赋值到给定位置上
        ((int16_t*)is->contents)[pos] = value;
        memrev16ifbe(((int16_t*)is->contents)+pos);
    }
}

//创建空的intSet集合
/* Create an empty intset. */
intset *intsetNew(void) {
    //分配内存
    intset *is = zmalloc(sizeof(intset));
    //设置初始编码为 int16
    is->encoding = intrev32ifbe(INTSET_ENC_INT16);
    //长度设置为 0
    is->length = 0;
    //返回集合的指针
    return is;
}

//集合扩容, len其实就是新的数组的长度
/* Resize the intset */
static intset *intsetResize(intset *is, uint32_t len) {
    //根据编码和长度, 计算出扩容后增加的内存大小
    uint32_t size = len*intrev32ifbe(is->encoding);
    //按原来的内存大小和增加的内存大小进行重分配
    is = zrealloc(is,sizeof(intset)+size);
    //返回集合指针
    return is;
}

/* Search for the position of "value". Return 1 when the value was found and
 * sets "pos" to the position of the value within the intset. Return 0 when
 * the value is not present in the intset and sets "pos" to the position
 * where "value" can be inserted. */
static uint8_t intsetSearch(intset *is, int64_t value, uint32_t *pos) {
    //初始化变量, max为数组长度减1
    int min = 0, max = intrev32ifbe(is->length)-1, mid = -1;
    int64_t cur = -1;

    /* The value can never be found when the set is empty */
    //如果集合中的数组为空,
    if (intrev32ifbe(is->length) == 0) {
        //如果需要返回, 则将找到的索引变成0
        if (pos) *pos = 0;
        //返回查找失败
        return 0;
    } else {
        /* Check for the case where we know we cannot find the value,
         * but do know the insert position. */
        //如果给定值大于intSet集合的最大值, 则返回查找失败
        if (value > _intsetGet(is,max)) {
            //设置返回的索引为数组的长度
            if (pos) *pos = intrev32ifbe(is->length);
            return 0;
            //给定值小于intSet集合的最小值
        } else if (value < _intsetGet(is,0)) {
            //设置返回索引为0
            if (pos) *pos = 0;
            return 0;
        }
    }

    //二分查询算法
    //如果最大索引大于最小索引, 则进行迭代
    while(max >= min) {
        //取一个中间索引 mid = (min + max)/2
        mid = ((unsigned int)min + (unsigned int)max) >> 1;
        //获取中间索引的值
        cur = _intsetGet(is,mid);
        //如果当前值比中间索引值大
        if (value > cur) {
            //取中间索引下一个索引做为最小索引
            min = mid+1;
            //如果当前值比中间值小
        } else if (value < cur) {
            //取中间索引的上一个索引为最大索引
            max = mid-1;
        } else {
            //值相等, 则直接退出循环
            break;
        }
    }

    //如果值与查到的值相等
    if (value == cur) {
        //设置查询的索引位置
        if (pos) *pos = mid;
        //返回查到成功
        return 1;
        //不相等, 则证明 min > max 退出循环
    } else {
        //设置查询到最大位置
        if (pos) *pos = min;
        //返回查找失败
        return 0;
    }
}

//升级集合编码并且插入元素.
/* Upgrades the intset to a larger encoding and inserts the given integer. */
static intset *intsetUpgradeAndAdd(intset *is, int64_t value) {
    //获取当前编码
    uint8_t curenc = intrev32ifbe(is->encoding);
    //根据值获取最新编码
    uint8_t newenc = _intsetValueEncoding(value);
    //获取数组的长度
    int length = intrev32ifbe(is->length);
    //因为外部调用已经确定是升级编码了, 所以value不是比当前intSet
    int prepend = value < 0 ? 1 : 0;

    /* First set new encoding and resize */
    //将集合改为新的编码
    is->encoding = intrev32ifbe(newenc);
    //对集合进行扩容. 传入的len是原数组长度 + 1
    is = intsetResize(is,intrev32ifbe(is->length)+1);

    /* Upgrade back-to-front so we don't overwrite values.
     * Note that the "prepend" variable is used to make sure we have an empty
     * space at either the beginning or the end of the intset. */
    while(length--)
        _intsetSet(is,length+prepend,_intsetGetEncoded(is,length,curenc));

    /* Set the value at the beginning or the end. */
    if (prepend)
        _intsetSet(is,0,value);
    else
        _intsetSet(is,intrev32ifbe(is->length),value);
    //设置intSet集合新的长度
    is->length = intrev32ifbe(intrev32ifbe(is->length)+1);
    //返回intSet集合
    return is;
}

//将from到end的数据移动到to之后
static void intsetMoveTail(intset *is, uint32_t from, uint32_t to) {
    void *src, *dst;
    //计算出要移动的元素个数
    uint32_t bytes = intrev32ifbe(is->length)-from;
    //获取intSet编码
    uint32_t encoding = intrev32ifbe(is->encoding);

    //处理int64
    if (encoding == INTSET_ENC_INT64) {
        //将数组强转成int64数组, 然后计算出from的位置
        src = (int64_t*)is->contents+from;
        //将数组强转成int64数组, 然后计算出to的位置
        dst = (int64_t*)is->contents+to;
        //根据元素个数和编码, 计算出要移动的内存大小
        bytes *= sizeof(int64_t);
        //处理int32
    } else if (encoding == INTSET_ENC_INT32) {
        src = (int32_t*)is->contents+from;
        dst = (int32_t*)is->contents+to;
        bytes *= sizeof(int32_t);
    } else {
        //处理int16
        src = (int16_t*)is->contents+from;
        dst = (int16_t*)is->contents+to;
        bytes *= sizeof(int16_t);
    }
    //移动内存
    memmove(dst,src,bytes);
}

/* Insert an integer in the intset */
//插入int数据到intSet集合中
intset *intsetAdd(intset *is, int64_t value, uint8_t *success) {
    //根据值获取合适的编码类型
    uint8_t valenc = _intsetValueEncoding(value);
    //value在集合中的位置
    uint32_t pos;
    //默认是成功的
    if (success) *success = 1;

    /* Upgrade encoding if necessary. If we need to upgrade, we know that
     * this value should be either appended (if > 0) or prepended (if < 0),
     * because it lies outside the range of existing values. */
    //如果编码大于当前集合的编码, 则升级编码再插入
    if (valenc > intrev32ifbe(is->encoding)) {
        /* This always succeeds, so we don't need to curry *success. */
        return intsetUpgradeAndAdd(is,value);
    } else {
        /* Abort if the value is already present in the set.
         * This call will populate "pos" with the right position to insert
         * the value when it cannot be found. */
        //编码不变, 则查询value在集合中的位置
        if (intsetSearch(is,value,&pos)) {
            //value已存在, 则返回插入失败
            if (success) *success = 0;
            return is;
        }

        //扩容集合
        is = intsetResize(is,intrev32ifbe(is->length)+1);
        //将pos+1位置之后的数据往后面移
        if (pos < intrev32ifbe(is->length)) intsetMoveTail(is,pos,pos+1);
    }

    //设置value的值
    _intsetSet(is,pos,value);
    //更新集合长度
    is->length = intrev32ifbe(intrev32ifbe(is->length)+1);
    return is;
}

/* Delete integer from intset */
//删除元素
intset *intsetRemove(intset *is, int64_t value, int *success) {
    //获取值的编码
    uint8_t valenc = _intsetValueEncoding(value);
    //元素的位置
    uint32_t pos;
    //是否成功, 默认是不成功的
    if (success) *success = 0;

    //如果编码比当前集合的编码小, 则表示有可能在集合中.
    //查询给定值, 如果给定值在集合中
    if (valenc <= intrev32ifbe(is->encoding) && intsetSearch(is,value,&pos)) {
        //获取集合的元素个数
        uint32_t len = intrev32ifbe(is->length);

        /* We know we can delete */
        //设置删除成功
        if (success) *success = 1;

        /* Overwrite value with tail and update length */
        //如果元素的位置在中间, 则将pos后面的元素往前移
        if (pos < (len-1)) intsetMoveTail(is,pos+1,pos);
        //集合缩容
        is = intsetResize(is,len-1);
        //设置元素个数减1
        is->length = intrev32ifbe(len-1);
    }
    return is;
}

/* Determine whether a value belongs to this set */
//判断元素是否在集合中
uint8_t intsetFind(intset *is, int64_t value) {
    //获取元素编码
    uint8_t valenc = _intsetValueEncoding(value);
    //如果值的大小在编码之内且存在于集合中, 则返回 true
    return valenc <= intrev32ifbe(is->encoding) && intsetSearch(is,value,NULL);
}

/* Return random member */
//随机获取集合中的值
int64_t intsetRandom(intset *is) {
    //获取集合元素个数
    uint32_t len = intrev32ifbe(is->length);
    assert(len); /* avoid division by zero on corrupt intset payload. */
    //生成随机数, 并且获取元素值
    return _intsetGet(is,rand()%len);
}

/* Get the value at the given position. When this position is
 * out of range the function returns 0, when in range it returns 1. */
//获取指定位置的元素
uint8_t intsetGet(intset *is, uint32_t pos, int64_t *value) {
    //如果位置没有超过数组大小
    if (pos < intrev32ifbe(is->length)) {
        //获取值, 并且放到value*指针
        *value = _intsetGet(is,pos);
        return 1;
    }
    return 0;
}

/* Return intset length */
//获取集合大小
uint32_t intsetLen(const intset *is) {
    return intrev32ifbe(is->length);
}

/* Return intset blob size in bytes. */
//获取集合内存大小
size_t intsetBlobLen(intset *is) {
    return sizeof(intset)+intrev32ifbe(is->length)*intrev32ifbe(is->encoding);
}

//校验数据的准确性
/* Validate the integrity of the data stracture.
 * when `deep` is 0, only the integrity of the header is validated.
 * when `deep` is 1, we make sure there are no duplicate or out of order records. */
int intsetValidateIntegrity(const unsigned char *p, size_t size, int deep) {
    //将指针强转为 intSet 集合
    intset *is = (intset *)p;
    /* check that we can actually read the header. */
    //如果给定大小小于intSet集合的最小大小, 则返回校验失败
    if (size < sizeof(*is))
        return 0;

    //获取集合编码
    uint32_t encoding = intrev32ifbe(is->encoding);

    //获取编码大小
    size_t record_size;
    if (encoding == INTSET_ENC_INT64) {
        record_size = INTSET_ENC_INT64;
    } else if (encoding == INTSET_ENC_INT32) {
        record_size = INTSET_ENC_INT32;
    } else if (encoding == INTSET_ENC_INT16){
        record_size = INTSET_ENC_INT16;
    } else {
        return 0;
    }

    /* check that the size matchies (all records are inside the buffer). */
    //获取数组长度
    uint32_t count = intrev32ifbe(is->length);
    //如果计算出的内存大小与给定的内存大小不一致, 则返回校验失败
    if (sizeof(*is) + count*record_size != size)
        return 0;

    /* check that the set is not empty. */
    //集合不能为空
    if (count==0)
        return 0;

    //如果不深度检验, 则返回校验成功
    if (!deep)
        return 1;

    /* check that there are no dup or out of order records. */
    //获取第0个元素
    int64_t prev = _intsetGet(is,0);
    //遍历集合, 校验每个元素都比前一个元素大
    for (uint32_t i=1; i<count; i++) {
        int64_t cur = _intsetGet(is,i);
        if (cur <= prev)
            return 0;
        prev = cur;
    }

    //返回校验成功
    return 1;
}

#ifdef REDIS_TEST
#include <sys/time.h>
#include <time.h>

#if 0
static void intsetRepr(intset *is) {
    for (uint32_t i = 0; i < intrev32ifbe(is->length); i++) {
        printf("%lld\n", (uint64_t)_intsetGet(is,i));
    }
    printf("\n");
}

static void error(char *err) {
    printf("%s\n", err);
    exit(1);
}
#endif

static void ok(void) {
    printf("OK\n");
}

static long long usec(void) {
    struct timeval tv;
    gettimeofday(&tv,NULL);
    return (((long long)tv.tv_sec)*1000000)+tv.tv_usec;
}

static intset *createSet(int bits, int size) {
    uint64_t mask = (1<<bits)-1;
    uint64_t value;
    intset *is = intsetNew();

    for (int i = 0; i < size; i++) {
        if (bits > 32) {
            value = (rand()*rand()) & mask;
        } else {
            value = rand() & mask;
        }
        is = intsetAdd(is,value,NULL);
    }
    return is;
}

static void checkConsistency(intset *is) {
    for (uint32_t i = 0; i < (intrev32ifbe(is->length)-1); i++) {
        uint32_t encoding = intrev32ifbe(is->encoding);

        if (encoding == INTSET_ENC_INT16) {
            int16_t *i16 = (int16_t*)is->contents;
            assert(i16[i] < i16[i+1]);
        } else if (encoding == INTSET_ENC_INT32) {
            int32_t *i32 = (int32_t*)is->contents;
            assert(i32[i] < i32[i+1]);
        } else {
            int64_t *i64 = (int64_t*)is->contents;
            assert(i64[i] < i64[i+1]);
        }
    }
}

#define UNUSED(x) (void)(x)
int intsetTest(int argc, char **argv) {
    uint8_t success;
    int i;
    intset *is;
    srand(time(NULL));

    UNUSED(argc);
    UNUSED(argv);

    printf("Value encodings: "); {
        assert(_intsetValueEncoding(-32768) == INTSET_ENC_INT16);
        assert(_intsetValueEncoding(+32767) == INTSET_ENC_INT16);
        assert(_intsetValueEncoding(-32769) == INTSET_ENC_INT32);
        assert(_intsetValueEncoding(+32768) == INTSET_ENC_INT32);
        assert(_intsetValueEncoding(-2147483648) == INTSET_ENC_INT32);
        assert(_intsetValueEncoding(+2147483647) == INTSET_ENC_INT32);
        assert(_intsetValueEncoding(-2147483649) == INTSET_ENC_INT64);
        assert(_intsetValueEncoding(+2147483648) == INTSET_ENC_INT64);
        assert(_intsetValueEncoding(-9223372036854775808ull) ==
                    INTSET_ENC_INT64);
        assert(_intsetValueEncoding(+9223372036854775807ull) ==
                    INTSET_ENC_INT64);
        ok();
    }

    printf("Basic adding: "); {
        is = intsetNew();
        is = intsetAdd(is,5,&success); assert(success);
        is = intsetAdd(is,6,&success); assert(success);
        is = intsetAdd(is,4,&success); assert(success);
        is = intsetAdd(is,4,&success); assert(!success);
        ok();
    }

    printf("Large number of random adds: "); {
        uint32_t inserts = 0;
        is = intsetNew();
        for (i = 0; i < 1024; i++) {
            is = intsetAdd(is,rand()%0x800,&success);
            if (success) inserts++;
        }
        assert(intrev32ifbe(is->length) == inserts);
        checkConsistency(is);
        ok();
    }

    printf("Upgrade from int16 to int32: "); {
        is = intsetNew();
        is = intsetAdd(is,32,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT16);
        is = intsetAdd(is,65535,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT32);
        assert(intsetFind(is,32));
        assert(intsetFind(is,65535));
        checkConsistency(is);

        is = intsetNew();
        is = intsetAdd(is,32,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT16);
        is = intsetAdd(is,-65535,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT32);
        assert(intsetFind(is,32));
        assert(intsetFind(is,-65535));
        checkConsistency(is);
        ok();
    }

    printf("Upgrade from int16 to int64: "); {
        is = intsetNew();
        is = intsetAdd(is,32,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT16);
        is = intsetAdd(is,4294967295,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT64);
        assert(intsetFind(is,32));
        assert(intsetFind(is,4294967295));
        checkConsistency(is);

        is = intsetNew();
        is = intsetAdd(is,32,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT16);
        is = intsetAdd(is,-4294967295,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT64);
        assert(intsetFind(is,32));
        assert(intsetFind(is,-4294967295));
        checkConsistency(is);
        ok();
    }

    printf("Upgrade from int32 to int64: "); {
        is = intsetNew();
        is = intsetAdd(is,65535,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT32);
        is = intsetAdd(is,4294967295,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT64);
        assert(intsetFind(is,65535));
        assert(intsetFind(is,4294967295));
        checkConsistency(is);

        is = intsetNew();
        is = intsetAdd(is,65535,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT32);
        is = intsetAdd(is,-4294967295,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT64);
        assert(intsetFind(is,65535));
        assert(intsetFind(is,-4294967295));
        checkConsistency(is);
        ok();
    }

    printf("Stress lookups: "); {
        long num = 100000, size = 10000;
        int i, bits = 20;
        long long start;
        is = createSet(bits,size);
        checkConsistency(is);

        start = usec();
        for (i = 0; i < num; i++) intsetSearch(is,rand() % ((1<<bits)-1),NULL);
        printf("%ld lookups, %ld element set, %lldusec\n",
               num,size,usec()-start);
    }

    printf("Stress add+delete: "); {
        int i, v1, v2;
        is = intsetNew();
        for (i = 0; i < 0xffff; i++) {
            v1 = rand() % 0xfff;
            is = intsetAdd(is,v1,NULL);
            assert(intsetFind(is,v1));

            v2 = rand() % 0xfff;
            is = intsetRemove(is,v2,NULL);
            assert(!intsetFind(is,v2));
        }
        checkConsistency(is);
        ok();
    }

    return 0;
}
#endif
