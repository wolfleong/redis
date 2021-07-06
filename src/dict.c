/* Hash Tables Implementation.
 *
 * This file implements in memory hash tables with insert/del/replace/find/
 * get-random-element operations. Hash tables will auto resize if needed
 * tables of power of two in size are used, collisions are handled by
 * chaining. See the source code for more information... :)
 *
 * Copyright (c) 2006-2012, Salvatore Sanfilippo <antirez at gmail dot com>
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

#include "fmacros.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <limits.h>
#include <sys/time.h>

#include "dict.h"
#include "zmalloc.h"
#ifndef DICT_BENCHMARK_MAIN
#include "redisassert.h"
#else
#include <assert.h>
#endif

//static 关键字的理解, https://zhuanlan.zhihu.com/p/112027143
//1. static 修饰局部变量, 此变量只能在作用域内访问, 但是变量存储在静态区, 函数执行完后变量不会销毁. 普通局部变量在函数执行完后, 立即销毁
//2. static 修饰全局变量, 表示此变量只能在当前文件内访问
//3. static 修饰函数, 表示此函数只能在当前文件内访问

/* Using dictEnableResize() / dictDisableResize() we make possible to
 * enable/disable resizing of the hash table as needed. This is very important
 * for Redis, as we use copy-on-write and don't want to move too much memory
 * around when there is a child performing saving operations.
 *
 * Note that even when dict_can_resize is set to 0, not all resizes are
 * prevented: a hash table is still allowed to grow if the ratio between
 * the number of elements and the buckets > dict_force_resize_ratio. */
//字典是否可以调整大小
static int dict_can_resize = 1;
//hash表扩容负载因子
static unsigned int dict_force_resize_ratio = 5;

/* -------------------------- private prototypes ---------------------------- */

//如果需要, 则进行扩容
static int _dictExpandIfNeeded(dict *ht);
static unsigned long _dictNextPower(unsigned long size);
//获取key在字典中的hash槽索引, 如果key已经存在, 则返回 -1, 并表设置在 *existing 指针中
static long _dictKeyIndex(dict *ht, const void *key, uint64_t hash, dictEntry **existing);
//初始化字典
static int _dictInit(dict *ht, dictType *type, void *privDataPtr);

/* -------------------------- hash functions -------------------------------- */

static uint8_t dict_hash_function_seed[16];

void dictSetHashFunctionSeed(uint8_t *seed) {
    memcpy(dict_hash_function_seed,seed,sizeof(dict_hash_function_seed));
}

uint8_t *dictGetHashFunctionSeed(void) {
    return dict_hash_function_seed;
}

/* The default hashing function uses SipHash implementation
 * in siphash.c. */

uint64_t siphash(const uint8_t *in, const size_t inlen, const uint8_t *k);
uint64_t siphash_nocase(const uint8_t *in, const size_t inlen, const uint8_t *k);

uint64_t dictGenHashFunction(const void *key, int len) {
    return siphash(key,len,dict_hash_function_seed);
}

uint64_t dictGenCaseHashFunction(const unsigned char *buf, int len) {
    return siphash_nocase(buf,len,dict_hash_function_seed);
}

/* ----------------------------- API implementation ------------------------- */

/* Reset a hash table already initialized with ht_init().
 * NOTE: This function should only be called by ht_destroy(). */
//hash字典重置
static void _dictReset(dictht *ht)
{
    ht->table = NULL;
    ht->size = 0;
    ht->sizemask = 0;
    ht->used = 0;
}

/* Create a new hash table */
//创建空字典的指针
dict *dictCreate(dictType *type,
        void *privDataPtr)
{
    //分配字典内存
    dict *d = zmalloc(sizeof(*d));

    //初始化字典
    _dictInit(d,type,privDataPtr);
    //返回字段指针
    return d;
}

/* Initialize the hash table */
//初始化hash表
int _dictInit(dict *d, dictType *type,
        void *privDataPtr)
{
    //重置第0个hash表
    _dictReset(&d->ht[0]);
    //重置第1个hash表
    _dictReset(&d->ht[1]);
    //设置字典类型
    d->type = type;
    //设置私有数据
    d->privdata = privDataPtr;
    //不进行rehash时, 为-1
    d->rehashidx = -1;
    d->pauserehash = 0;
    return DICT_OK;
}

/* Resize the table to the minimal size that contains all the elements,
 * but with the invariant of a USED/BUCKETS ratio near to <= 1 */
//字典表调整大小
int dictResize(dict *d)
{
    //字段数组最小长度
    unsigned long minimal;

    //如果字段不可以调整大小或正在rehash, 则返回错误
    if (!dict_can_resize || dictIsRehashing(d)) return DICT_ERR;
    //获取hash表的元素数量
    minimal = d->ht[0].used;
    //如果元素数量小于最小初始化大小
    if (minimal < DICT_HT_INITIAL_SIZE)
        //则默认用初始化大小
        minimal = DICT_HT_INITIAL_SIZE;
    //字典扩容
    return dictExpand(d, minimal);
}

/* Expand or create the hash table,
 * when malloc_failed is non-NULL, it'll avoid panic if malloc fails (in which case it'll be set to 1).
 * Returns DICT_OK if expand was performed, and DICT_ERR if skipped. */
//扩容或者创建字典
int _dictExpand(dict *d, unsigned long size, int* malloc_failed)
{
    if (malloc_failed) *malloc_failed = 0;

    /* the size is invalid if it is smaller than the number of
     * elements already inside the hash table */
    //如果字典正在rehash, 或者已有节点数大于扩容数组的大小, 则直接返回错误
    if (dictIsRehashing(d) || d->ht[0].used > size)
        return DICT_ERR;

    dictht n; /* the new hash table */
    //计算要扩容的容量
    unsigned long realsize = _dictNextPower(size);

    /* Rehashing to the same table size is not useful. */
    //如果要扩容的容量跟原来的一样, 则返回错误
    if (realsize == d->ht[0].size) return DICT_ERR;

    /* Allocate the new hash table and initialize all pointers to NULL */
    //设置新的容量
    n.size = realsize;
    n.sizemask = realsize-1;
    //如果要避免分配失败报错
    if (malloc_failed) {
        //尝试分配hash数组
        n.table = ztrycalloc(realsize*sizeof(dictEntry*));
        //设置是否分配失败, n.table == NULL 就是分配失败
        *malloc_failed = n.table == NULL;
        //如果分配失败, 则返回错误
        if (*malloc_failed)
            return DICT_ERR;
    } else
        //分配hash数组, 分配失败则终止程序
        n.table = zcalloc(realsize*sizeof(dictEntry*));

    //设置节点数为0
    n.used = 0;

    /* Is this the first initialization? If so it's not really a rehashing
     * we just set the first hash table so that it can accept keys. */
    //如果字典的hash表为 NULL, 则表示这是首次初始化
    if (d->ht[0].table == NULL) {
        //将hash表放到hash数组中第0位中
        d->ht[0] = n;
        return DICT_OK;
    }

    //如果非首次初始化, 则放到hash数组第1位, 用于rehash
    /* Prepare a second hash table for incremental rehashing */
    d->ht[1] = n;
    //设置从hash表中的第0位进行迁移
    d->rehashidx = 0;
    //返回成功
    return DICT_OK;
}

//如果扩容失败, 报内存溢出
/* return DICT_ERR if expand was not performed */
int dictExpand(dict *d, unsigned long size) {
    return _dictExpand(d, size, NULL);
}

//字典尝试扩容, 扩容失败则返回错误
/* return DICT_ERR if expand failed due to memory allocation failure */
int dictTryExpand(dict *d, unsigned long size) {
    //是否失败
    int malloc_failed;
    //扩容
    _dictExpand(d, size, &malloc_failed);
    //如果失败, 则返回错误, 否则返回成功
    return malloc_failed? DICT_ERR : DICT_OK;
}

/* Performs N steps of incremental rehashing. Returns 1 if there are still
 * keys to move from the old to the new hash table, otherwise 0 is returned.
 *
 * Note that a rehashing step consists in moving a bucket (that may have more
 * than one key as we use chaining) from the old to the new hash table, however
 * since part of the hash table may be composed of empty spaces, it is not
 * guaranteed that this function will rehash even a single bucket, since it
 * will visit at max N*10 empty buckets in total, otherwise the amount of
 * work it does would be unbound and the function may block for a long time. */
//字典 rehash 过程, n 表示迁移桶的数量
int dictRehash(dict *d, int n) {
    //最多遍历空桶的数量
    int empty_visits = n*10; /* Max number of empty buckets to visit. */
    //如果字典不是rehash状态, 直接返回迁移失败
    if (!dictIsRehashing(d)) return 0;

    //如果迁移桶的数量不为空, 且hash表没迁移完. 注意: 这里是先读取n再--
    while(n-- && d->ht[0].used != 0) {
        dictEntry *de, *nextde;

        /* Note that rehashidx can't overflow as we are sure there are more
         * elements because ht[0].used != 0 */
        //断言hash表桶的个数是大于已经rehash的桶索引. rehashidx 最大为 ht[0].size - 1
        assert(d->ht[0].size > (unsigned long)d->rehashidx);
        //如果要迁移的桶的链表首节点为 NULL, 则表示此桶为空. 这个循环主要上去掉空桶
        while(d->ht[0].table[d->rehashidx] == NULL) {
            //准备迁移下一个桶
            d->rehashidx++;
            //如果已经超过规定处理的最大空桶数量, 则直接返回迁移成功
            if (--empty_visits == 0) return 1;
        }
        //获取rehash上面的链表首节点
        de = d->ht[0].table[d->rehashidx];
        /* Move all the keys in this bucket from the old to the new hash HT */
        //遍历链表
        while(de) {
            uint64_t h;

            //获取下一个节点
            nextde = de->next;
            /* Get the index in the new hash table */
            //重新计算当前节点在新的hash表中的槽的索引
            h = dictHashKey(d, de->key) & d->ht[1].sizemask;
            //将节点设置到新的hash表中
            de->next = d->ht[1].table[h];
            d->ht[1].table[h] = de;
            //旧hash表数量减少
            d->ht[0].used--;
            //新hash表数量增加
            d->ht[1].used++;
            //攻取下一个节点
            de = nextde;
        }
        //清空旧节点的hash槽
        d->ht[0].table[d->rehashidx] = NULL;
        //设置下一个要迁移处理的hash槽
        d->rehashidx++;
    }

    /* Check if we already rehashed the whole table... */
    //处理完, 判断是一下hash表是否迁移完成
    if (d->ht[0].used == 0) {
        //迁移完成
        //回收旧hash表的数组
        zfree(d->ht[0].table);
        //用新hash表替换旧hash表, 赋值相当于拷贝
        d->ht[0] = d->ht[1];
        //重置临时的hash表
        _dictReset(&d->ht[1]);
        //清空下一个rehash的索引
        d->rehashidx = -1;
        return 0;
    }

    /* More to rehash... */
    return 1;
}

//获取当前毫秒时间
long long timeInMilliseconds(void) {
    struct timeval tv;

    gettimeofday(&tv,NULL);
    return (((long long)tv.tv_sec)*1000)+(tv.tv_usec/1000);
}

/* Rehash in ms+"delta" milliseconds. The value of "delta" is larger 
 * than 0, and is smaller than 1 in most cases. The exact upper bound 
 * depends on the running time of dictRehash(d,100).*/
int dictRehashMilliseconds(dict *d, int ms) {
    //如果rehash是暂停的, 则不处理
    if (d->pauserehash > 0) return 0;

    //获取当前时间戳
    long long start = timeInMilliseconds();
    //统计rehash桶的个数
    int rehashes = 0;

    //rehash 100 个桶
    while(dictRehash(d,100)) {
        //增加统计
        rehashes += 100;
        //如果执行时间大于给定的时间, 则退出
        if (timeInMilliseconds()-start > ms) break;
    }
    //返回rehash桶的个数
    return rehashes;
}

/* This function performs just a step of rehashing, and only if hashing has
 * not been paused for our hash table. When we have iterators in the
 * middle of a rehashing we can't mess with the two hash tables otherwise
 * some element can be missed or duplicated.
 *
 * This function is called by common lookup or update operations in the
 * dictionary so that the hash table automatically migrates from H1 to H2
 * while it is actively used. */
//执行一步渐进式rehash
static void _dictRehashStep(dict *d) {
    //如果rehash不是暂停的, 则执行rehash迁移
    if (d->pauserehash == 0) dictRehash(d,1);
}

//添加kv对
/* Add an element to the target hash table */
int dictAdd(dict *d, void *key, void *val)
{
    //添加一个节点到字段中, 并且返回这个节点
    dictEntry *entry = dictAddRaw(d,key,NULL);

    //如果添加节点不成功, 则返回错误
    if (!entry) return DICT_ERR;
    //将值设置到 entry 中
    dictSetVal(d, entry, val);
    //返回成功
    return DICT_OK;
}

/* Low level add or find:
 * This function adds the entry but instead of setting a value returns the
 * dictEntry structure to the user, that will make sure to fill the value
 * field as they wish.
 *
 * This function is also directly exposed to the user API to be called
 * mainly in order to store non-pointers inside the hash value, example:
 *
 * entry = dictAddRaw(dict,mykey,NULL);
 * if (entry != NULL) dictSetSignedIntegerVal(entry,1000);
 *
 * Return values:
 *
 * If key already exists NULL is returned, and "*existing" is populated
 * with the existing entry if existing is not NULL.
 *
 * If key was added, the hash entry is returned to be manipulated by the caller.
 */
//创建新的节点
dictEntry *dictAddRaw(dict *d, void *key, dictEntry **existing)
{
    long index;
    dictEntry *entry;
    dictht *ht;

    //如果字典在rehash, 则在处理请求前先搬一部分的key
    if (dictIsRehashing(d)) _dictRehashStep(d);

    /* Get the index of the new element, or -1 if
     * the element already exists. */
    //获取 key 对应的槽索引, 如果key已经存在, 则返回 -1
    //如果key已经存在, 则返回 NULL
    if ((index = _dictKeyIndex(d, key, dictHashKey(d,key), existing)) == -1)
        return NULL;

    /* Allocate the memory and store the new entry.
     * Insert the element in top, with the assumption that in a database
     * system it is more likely that recently added entries are accessed
     * more frequently. */
    //获取hash表, 如果正在rehash, 则用新的hash表, 否则用旧的hash表
    ht = dictIsRehashing(d) ? &d->ht[1] : &d->ht[0];
    //分配 dictEntry 内存
    entry = zmalloc(sizeof(*entry));
    //添加节点到槽的链表头
    entry->next = ht->table[index];
    //将新建的节点放入hash槽中
    ht->table[index] = entry;
    //节点数量加1
    ht->used++;

    /* Set the hash entry fields. */
    //设置 key 到节点中
    dictSetKey(d, entry, key);
    return entry;
}

/* Add or Overwrite:
 * Add an element, discarding the old value if the key already exists.
 * Return 1 if the key was added from scratch, 0 if there was already an
 * element with such key and dictReplace() just performed a value update
 * operation. */
//添加kv, 如果存在则覆盖
int dictReplace(dict *d, void *key, void *val)
{
    dictEntry *entry, *existing, auxentry;

    /* Try to add the element. If the key
     * does not exists dictAdd will succeed. */
    //添加节点
    entry = dictAddRaw(d,key,&existing);
    //如果添加成功
    if (entry) {
        //将值设置到节点中, 返回成功
        dictSetVal(d, entry, val);
        return 1;
    }

    /* Set the new value and free the old one. Note that it is important
     * to do that in this order, as the value may just be exactly the same
     * as the previous one. In this context, think to reference counting,
     * you want to increment (set), and then decrement (free), and not the
     * reverse. */
    //获取旧的节点, 结构体赋值相当于浅拷贝
    auxentry = *existing;

    //设置新的值
    dictSetVal(d, existing, val);
    //释放旧的值
    dictFreeVal(d, &auxentry);
    return 0;
}

/* Add or Find:
 * dictAddOrFind() is simply a version of dictAddRaw() that always
 * returns the hash entry of the specified key, even if the key already
 * exists and can't be added (in that case the entry of the already
 * existing key is returned.)
 *
 * See dictAddRaw() for more information. */
//添加节点, 如果已存在, 则直接返回已存在的节点
dictEntry *dictAddOrFind(dict *d, void *key) {
    dictEntry *entry, *existing;
    entry = dictAddRaw(d,key,&existing);
    return entry ? entry : existing;
}

/* Search and remove an element. This is an helper function for
 * dictDelete() and dictUnlink(), please check the top comment
 * of those functions. */
//删除一个节点, nofree 表示不回收删除节点的内存
static dictEntry *dictGenericDelete(dict *d, const void *key, int nofree) {
    uint64_t h, idx;
    dictEntry *he, *prevHe;
    int table;

    //如果hash表为空, 则直接返回 NULL
    if (d->ht[0].used == 0 && d->ht[1].used == 0) return NULL;

    //如果hash表正在rehash, 则执行rehash处理
    if (dictIsRehashing(d)) _dictRehashStep(d);
    //计算出key的hash值
    h = dictHashKey(d, key);

    //遍历hash表
    for (table = 0; table <= 1; table++) {
        //根据掩码, 计算出hash槽的索引
        idx = h & d->ht[table].sizemask;
        //获取hash槽的链表首节点
        he = d->ht[table].table[idx];
        //前一个节点引用设置为 NULL, 这个用于记录上一个节点
        prevHe = NULL;
        //如果首节点存在
        while(he) {
            //如果节点的key跟传入的key相等或者调用比较函数比较相等
            if (key==he->key || dictCompareKeys(d, key, he->key)) {
                /* Unlink the element from the list */
                //下面if..else主要是目标是将 he 从链表中移除
                //如果上一个节点存在, 则将next指针指向he的next
                if (prevHe)
                    prevHe->next = he->next;
                else
                    //prevHe为NULL, 则表示直接用he的next作为首节点
                    d->ht[table].table[idx] = he->next;
                //如果要释放节点的内存
                if (!nofree) {
                    //回收key的内存
                    dictFreeKey(d, he);
                    //回收val的内存
                    dictFreeVal(d, he);
                    //回收dictEntry的内存
                    zfree(he);
                }
                //减少节点个数
                d->ht[table].used--;
                //返回删除的节点
                return he;
            }
            //记录当前节点为前一个节点
            prevHe = he;
            //获取下一个节点
            he = he->next;
        }
        //如果字典不是 rehash, 则退出循环
        if (!dictIsRehashing(d)) break;
    }
    //没找到, 则返回 NULL
    return NULL; /* not found */
}

/* Remove an element, returning DICT_OK on success or DICT_ERR if the
 * element was not found. */
//删除指定key
int dictDelete(dict *ht, const void *key) {
    return dictGenericDelete(ht,key,0) ? DICT_OK : DICT_ERR;
}

/* Remove an element from the table, but without actually releasing
 * the key, value and dictionary entry. The dictionary entry is returned
 * if the element was found (and unlinked from the table), and the user
 * should later call `dictFreeUnlinkedEntry()` with it in order to release it.
 * Otherwise if the key is not found, NULL is returned.
 *
 * This function is useful when we want to remove something from the hash
 * table but want to use its value before actually deleting the entry.
 * Without this function the pattern would require two lookups:
 *
 *  entry = dictFind(...);
 *  // Do something with entry
 *  dictDelete(dictionary,entry);
 *
 * Thanks to this function it is possible to avoid this, and use
 * instead:
 *
 * entry = dictUnlink(dictionary,entry);
 * // Do something with entry
 * dictFreeUnlinkedEntry(entry); // <- This does not need to lookup again.
 */
//移除字典的节点, 不回收内存
dictEntry *dictUnlink(dict *ht, const void *key) {
    return dictGenericDelete(ht,key,1);
}

/* You need to call this function to really free the entry after a call
 * to dictUnlink(). It's safe to call this function with 'he' = NULL. */
//回收节点的内存
void dictFreeUnlinkedEntry(dict *d, dictEntry *he) {
    //如果节点为 NULL, 则直接返回
    if (he == NULL) return;
    //回收 key 的内存
    dictFreeKey(d, he);
    //回收 value 的内存
    dictFreeVal(d, he);
    //回收节点的内存
    zfree(he);
}

/* Destroy an entire dictionary */
//清空字典中的hash表
int _dictClear(dict *d, dictht *ht, void(callback)(void *)) {
    unsigned long i;

    /* Free all the elements */
    //如果hash表的hash数组已经初始化且hash表中有元素, 则遍历处理
    for (i = 0; i < ht->size && ht->used > 0; i++) {
        //he 当前节点, nextHe 表示下一个节点
        dictEntry *he, *nextHe;

        //如果有传callback函数, 则每清空65535个槽, 则执行回调方法一次
        //为什么要这么处理这个回调呢 ? 主要为了避免hash表太大, 一直删除会阻塞, 通过回调方法, 删除阻塞过程中能够处理新的请求 https://www.modb.pro/db/72930
        if (callback && (i & 65535) == 0) callback(d->privdata);

        //获取首节点不存在, 如果首节点不存在则不处理
        if ((he = ht->table[i]) == NULL) continue;
        //如果节点存在
        while(he) {
            //获取下一个节点
            nextHe = he->next;
            //回收当前节点key的内存
            dictFreeKey(d, he);
            //回收当前节点val的内存
            dictFreeVal(d, he);
            //回收节点的内存
            zfree(he);
            //减少节点数量
            ht->used--;
            //获取下一个节点
            he = nextHe;
        }
    }
    /* Free the table and the allocated cache structure */
    //释放hash表
    zfree(ht->table);
    /* Re-initialize the table */
    //重置字典
    _dictReset(ht);
    //返回 OK
    return DICT_OK; /* never fails */
}

/* Clear & Release the hash table */
//回收字典内存
void dictRelease(dict *d)
{
    //清空hash表
    _dictClear(d,&d->ht[0],NULL);
    //清空rehash中的hash表
    _dictClear(d,&d->ht[1],NULL);
    //回收字典内存
    zfree(d);
}

//根据key查询节点
dictEntry *dictFind(dict *d, const void *key)
{
    dictEntry *he;
    uint64_t h, idx, table;

    //如果字典为空, 则直接返回 NULL
    if (dictSize(d) == 0) return NULL; /* dict is empty */
    //如果字典正在 rehash, 则执行一次rehash步聚
    if (dictIsRehashing(d)) _dictRehashStep(d);
    //计算key的hash值
    h = dictHashKey(d, key);
    //遍历字典的hash表
    for (table = 0; table <= 1; table++) {
        //计算出key所有的hash槽索引
        idx = h & d->ht[table].sizemask;
        //根据hash槽获取首节点
        he = d->ht[table].table[idx];
        //遍历链表, 直到找到key一样的节点
        while(he) {
            //如果找到, 则直接返回找到的节点
            if (key==he->key || dictCompareKeys(d, key, he->key))
                return he;
            he = he->next;
        }
        //如果没找到, 且字典没有rehash, 则直接返回 NULL
        if (!dictIsRehashing(d)) return NULL;
    }
    //没找到, 返回NULL
    return NULL;
}

//获取key对应的值
void *dictFetchValue(dict *d, const void *key) {
    dictEntry *he;

    //根据key查询节点
    he = dictFind(d,key);
    //如果节点存在, 则获取节点的值, 否则返回 NULL
    return he ? dictGetVal(he) : NULL;
}

/* A fingerprint is a 64 bit number that represents the state of the dictionary
 * at a given time, it's just a few dict properties xored together.
 * When an unsafe iterator is initialized, we get the dict fingerprint, and check
 * the fingerprint again when the iterator is released.
 * If the two fingerprints are different it means that the user of the iterator
 * performed forbidden operations against the dictionary while iterating. */
//计算字典当前状态的签名
long long dictFingerprint(dict *d) {
    long long integers[6], hash = 0;
    int j;

    //字典所有的内存值, 按顺序获取内存大小
    integers[0] = (long) d->ht[0].table;
    integers[1] = d->ht[0].size;
    integers[2] = d->ht[0].used;
    integers[3] = (long) d->ht[1].table;
    integers[4] = d->ht[1].size;
    integers[5] = d->ht[1].used;

    /* We hash N integers by summing every successive integer with the integer
     * hashing of the previous sum. Basically:
     *
     * Result = hash(hash(hash(int1)+int2)+int3) ...
     *
     * This way the same set of integers in a different order will (likely) hash
     * to a different number. */
    //遍历内存值, 计算出一个hash值
    for (j = 0; j < 6; j++) {
        hash += integers[j];
        /* For the hashing step we use Tomas Wang's 64 bit integer hash. */
        hash = (~hash) + (hash << 21); // hash = (hash << 21) - hash - 1;
        hash = hash ^ (hash >> 24);
        hash = (hash + (hash << 3)) + (hash << 8); // hash * 265
        hash = hash ^ (hash >> 14);
        hash = (hash + (hash << 2)) + (hash << 4); // hash * 21
        hash = hash ^ (hash >> 28);
        hash = hash + (hash << 31);
    }
    return hash;
}

//获取字典迭代器
dictIterator *dictGetIterator(dict *d)
{
    //分配迭代器内存
    dictIterator *iter = zmalloc(sizeof(*iter));

    //迭代器初始化
    iter->d = d;
    iter->table = 0;
    iter->index = -1;
    iter->safe = 0;
    iter->entry = NULL;
    iter->nextEntry = NULL;
    return iter;
}

//获取安全的迭代器
dictIterator *dictGetSafeIterator(dict *d) {
    //获取迭代器
    dictIterator *i = dictGetIterator(d);

    //设置save为1
    i->safe = 1;
    return i;
}

//获取下一个节点
dictEntry *dictNext(dictIterator *iter)
{
    //死循环
    while (1) {
        //如果当前节点为空
        if (iter->entry == NULL) {
            //获取要遍历的hash表
            dictht *ht = &iter->d->ht[iter->table];
            //如果hash数组table的索引为-1或者ht数组索引为0, 则表示第一次进来遍历
            if (iter->index == -1 && iter->table == 0) {
                //如果是安全的遍历, 则要暂停rehash
                if (iter->safe)
                    dictPauseRehashing(iter->d);
                else
                    //不安全的遍历, 则给字典计算一个指纹(相当于校验和)
                    iter->fingerprint = dictFingerprint(iter->d);
            }
            //索引加1
            iter->index++;
            //如果当前索引大于hash数组长度
            if (iter->index >= (long) ht->size) {
                //判断当前字典是不是在rehash, 并且还没有把所有hash表遍历完
                if (dictIsRehashing(iter->d) && iter->table == 0) {
                    //索引加1, 遍历一下个hash表
                    iter->table++;
                    //hash表的索引重置为0
                    iter->index = 0;
                    //获取第二个hash表
                    ht = &iter->d->ht[1];
                } else {
                    break;
                }
            }
            //获取hash槽的首节点
            iter->entry = ht->table[iter->index];
        } else {
            //获取下一个节点
            iter->entry = iter->nextEntry;
        }
        //如果节点不为NULL
        if (iter->entry) {
            /* We need to save the 'next' here, the iterator user
             * may delete the entry we are returning. */
            //先记录下一个节点, 方便使用. 当前节点 entry 有可能被删除
            iter->nextEntry = iter->entry->next;
            //返回当前节点
            return iter->entry;
        }
    }
    //没找到, 则返回 NULL
    return NULL;
}

//回收迭代器
void dictReleaseIterator(dictIterator *iter)
{
    //如果迭代器已经开始遍历
    if (!(iter->index == -1 && iter->table == 0)) {
        //判断是否按全
        if (iter->safe)
            //减少rehash停顿数量
            dictResumeRehashing(iter->d);
        else
            //断言当前签名与迭代器记录的状态必须一致, 不一致表示用户进行了非法操作
            assert(iter->fingerprint == dictFingerprint(iter->d));
    }
    //回收迭代器的内存
    zfree(iter);
}

/* Return a random entry from the hash table. Useful to
 * implement randomized algorithms */
//从字典中获取一个随机的key. 可能会出现不公平的情况
//算法大概的思路是: 随机获取一个hash槽, 然后从槽的链表中随机获取一个节点
dictEntry *dictGetRandomKey(dict *d)
{
    dictEntry *he, *orighe;
    unsigned long h;
    //listlen 链表长度, listele 链表随机的位置
    int listlen, listele;

    //如果字典为空, 则直接返回 NULL
    if (dictSize(d) == 0) return NULL;
    //如果字典正在 rehash, 则执行rehash的迁移
    if (dictIsRehashing(d)) _dictRehashStep(d);
    //如果当前字典正在rehash
    if (dictIsRehashing(d)) {
        //循环随机获取, 直接到hash槽有节点存在为止
        do {
            /* We are sure there are no elements in indexes from 0
             * to rehashidx-1 */
            //获取一个随机数, 然后根据两个hash表的长度计算hash槽.
            // randomULong() % (dictSlots(d) - d->rehashidx) 保证随机值不包括 rehashidx 之前的, 注意, 这里是取模不是&
            h = d->rehashidx + (randomULong() % (dictSlots(d) - d->rehashidx));
            //如果算出来的随机hash槽大于旧hash表的长度, 则表示要获取新hash表的随机槽首节点, 否则获取旧hash表的随机槽首节点
            he = (h >= d->ht[0].size) ? d->ht[1].table[h - d->ht[0].size] :
                                      d->ht[0].table[h];
        } while(he == NULL);
    } else {
        //不在rehash, 只有一个hash表
        do {
            //生成随机数, 计算随机hash槽
            h = randomULong() & d->ht[0].sizemask;
            //获取随机hash槽的首节点
            he = d->ht[0].table[h];
            //节点为NULL, 则继续随机
        } while(he == NULL);
    }

    /* Now we found a non empty bucket, but it is a linked
     * list and we need to get a random element from the list.
     * The only sane way to do so is counting the elements and
     * select a random index. */
    //链表长度设置为0
    listlen = 0;
    //先将首节点暂存起来
    orighe = he;
    //遍历所有节点, 统计链表长度
    while(he) {
        he = he->next;
        listlen++;
    }
    //随机获取链表上的位置
    listele = random() % listlen;
    //遍历链表, 获取指定位置的节点
    he = orighe;
    while(listele--) he = he->next;
    //返回随机的节点
    return he;
}

/* This function samples the dictionary to return a few keys from random
 * locations.
 *
 * It does not guarantee to return all the keys specified in 'count', nor
 * it does guarantee to return non-duplicated elements, however it will make
 * some effort to do both things.
 *
 * Returned pointers to hash table entries are stored into 'des' that
 * points to an array of dictEntry pointers. The array must have room for
 * at least 'count' elements, that is the argument we pass to the function
 * to tell how many random elements we need.
 *
 * The function returns the number of items stored into 'des', that may
 * be less than 'count' if the hash table has less than 'count' elements
 * inside, or if not enough elements were found in a reasonable amount of
 * steps.
 *
 * Note that this function is not suitable when you need a good distribution
 * of the returned items, but only when you need to "sample" a given number
 * of continuous elements to run some kind of algorithm or to produce
 * statistics. However the function is much faster than dictGetRandomKey()
 * at producing N elements. */
//随机采集指定数量的节点. 有可能返回的数量达不到 count 的个数. 如果要返回一些随机key, 这个函数比 dictGetRandomKey 快很多
unsigned int dictGetSomeKeys(dict *d, dictEntry **des, unsigned int count) {
    unsigned long j; /* internal hash table id, 0 or 1. */
    //hash表的数量, 值为1或者2
    unsigned long tables; /* 1 or 2 tables? */
    //stored 表示已经采集的节点数, maxsizemask 表示容量的最大hash表的掩码
    unsigned long stored = 0, maxsizemask;
    //采集次数上限
    unsigned long maxsteps;

    //最多只能返回字典的总节点数.
    if (dictSize(d) < count) count = dictSize(d);
    //采集次数上限为元素个数的10倍
    maxsteps = count*10;

    /* Try to do a rehashing work proportional to 'count'. */
    //根据返回key的个数, 执行渐进式rehash操作
    for (j = 0; j < count; j++) {
        if (dictIsRehashing(d))
            _dictRehashStep(d);
        else
            break;
    }

    //如果字典正在rehash, 则需要遍历两个hash表, 否则就遍历一个
    tables = dictIsRehashing(d) ? 2 : 1;
    //获取hash表0的掩码作为最大掩码
    maxsizemask = d->ht[0].sizemask;
    //如果hash表数量大于1, 表示字典现在是rehash状态
    //如果字典是rehash状态, 则对比两个hash表的掩码, 取最大的作为 maxsizemask
    if (tables > 1 && maxsizemask < d->ht[1].sizemask)
        maxsizemask = d->ht[1].sizemask;

    /* Pick a random point inside the larger table. */
    //获取随机数然后计算出一个随机hash槽.
    unsigned long i = randomULong() & maxsizemask;
    //统计遍历了空的hash槽个数
    unsigned long emptylen = 0; /* Continuous empty entries so far. */
    //如果采样的key已经足够或者达到采样上限, 则退出循环
    while(stored < count && maxsteps--) {
        //遍历hash表数组进行采集
        for (j = 0; j < tables; j++) {
            /* Invariant of the dict.c rehashing: up to the indexes already
             * visited in ht[0] during the rehashing, there are no populated
             * buckets, so we can skip ht[0] for indexes between 0 and idx-1. */
            //跳过已经迁移到新hash表的hash槽索引
            //tables == 2 字典表示正在rehash
            //j == 0 , 表示目前正在遍历旧hash表
            //i < (unsigned long) d->rehashidx 表示i属于已经迁移的hash槽索引
            if (tables == 2 && j == 0 && i < (unsigned long) d->rehashidx) {
                /* Moreover, if we are currently out of range in the second
                 * table, there will be no elements in both tables up to
                 * the current rehashing index, so we jump if possible.
                 * (this happens when going from big to small table). */
                //如果当前随机索引大于hash表1的长度, 表示只能在hash表0中获取, 那么跳过 rehashidx 前面已经迁移的槽
                if (i >= d->ht[1].size)
                    i = d->rehashidx;
                else
                    //i 小于 rehashidx, 但是没有大于hash表1的容量, 直接跳过hash表0, 从hash表1中采样
                    continue;
            }
            //如果随机hash槽索引大于当前hash表数组的长度, 则不处理
            if (i >= d->ht[j].size) continue; /* Out of range for this table. */
            //获取hash表的首节点
            dictEntry *he = d->ht[j].table[i];

            /* Count contiguous empty buckets, and jump to other
             * locations if they reach 'count' (with a minimum of 5). */
            //如果首节点为 NULL
            if (he == NULL) {
                //统计空的hash槽
                emptylen++;
                //如果空的hash槽个数超过5且超过 count, 重新生成随机hash槽索引, 并且重置空的hash槽统计
                if (emptylen >= 5 && emptylen > count) {
                    i = randomULong() & maxsizemask;
                    emptylen = 0;
                }
            } else {
                //首节点不为空, 重置空的hash槽统计
                emptylen = 0;
                //遍历链表
                while (he) {
                    /* Collect all the elements of the buckets found non
                     * empty while iterating. */
                    //将节点放进 dictEntry * 数组
                    *des = he;
                    //数组指针移动到下一个索引
                    des++;
                    //获取下一个节点
                    he = he->next;
                    //获取的节点数加1
                    stored++;
                    //如果获取的节点数已经满足, 则直接反回
                    if (stored == count) return stored;
                }
            }
        }
        //获取下一个hash槽位置
        i = (i+1) & maxsizemask;
    }
    return stored;
}

/* This is like dictGetRandomKey() from the POV of the API, but will do more
 * work to ensure a better distribution of the returned element.
 *
 * This function improves the distribution because the dictGetRandomKey()
 * problem is that it selects a random bucket, then it selects a random
 * element from the chain in the bucket. However elements being in different
 * chain lengths will have different probabilities of being reported. With
 * this function instead what we do is to consider a "linear" range of the table
 * that may be constituted of N buckets with chains of different lengths
 * appearing one after the other. Then we report a random element in the range.
 * In this way we smooth away the problem of different chain lengths. */
#define GETFAIR_NUM_ENTRIES 15
//公平地获取一个随机key.
//为什么比 dictGetRandomKey 公平一点呢?. dictGetRandomKey 由于不同的槽, 链表的长度可能不一样, 就会导致概率的分布不一样
//dictGetSomeKeys 返回的长度是固定的, 从固定的链表长度中随机节点, 相对于 dictGetRandomKey 链表长度不固定会公平一点
dictEntry *dictGetFairRandomKey(dict *d) {
    //节点数组
    dictEntry *entries[GETFAIR_NUM_ENTRIES];
    //随机获取15个节点
    unsigned int count = dictGetSomeKeys(d,entries,GETFAIR_NUM_ENTRIES);
    /* Note that dictGetSomeKeys() may return zero elements in an unlucky
     * run() even if there are actually elements inside the hash table. So
     * when we get zero, we call the true dictGetRandomKey() that will always
     * yield the element if the hash table has at least one. */
    //如果没有获取到, 则随机返回一个key
    if (count == 0) return dictGetRandomKey(d);
    //在这一组节点中, 生成随机索引
    unsigned int idx = rand() % count;
    //在这一组节点中, 随机获取一个
    return entries[idx];
}

/* Function to reverse bits. Algorithm from:
 * http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel */
// 对 v 进行二进制逆序操作, 这个算法有点意思, 可以看一下 https://www.cnblogs.com/gqtcgq/p/7247077.html
static unsigned long rev(unsigned long v) {
    //CHAR_BIT 一般是8位, sizeof 表示v的字节, long是4个字节, 也就是 s=32, 也就是二进制的 100000
    unsigned long s = CHAR_BIT * sizeof(v); // bit size; must be power of 2
    //~0UL 相当于32个1
    unsigned long mask = ~0UL;
    //s >>= 1 第一次移动之后就是 010000, 也就是16, 第三次为8, 依次类推4, 2, 1, 最多向右移动6次就为0了, 也就是说while有5次的遍历操作
    while ((s >>= 1) > 0) {
        //mask << s相当于左移s位, 也就是保留低s位, 最终变成高s位为1, 低s位为0
        //mask ^= (mask << s), mask结果为只有低s位都为1, 高s位都是0, 如: 也就是高16位都为0, 低16位都为1
        mask ^= (mask << s);
        //(v >> s) & mask相当于将高s位移动到低s位
        //~mask表示高s位都是1, 低s位都是0, (v << s) & ~mask 相当将低s位移动到高s位
        //将两者 | , 表示将高s位与低s位互换了
        v = ((v >> s) & mask) | ((v << s) & ~mask);
    }
    return v;
}

/* dictScan() is used to iterate over the elements of a dictionary.
 *
 * Iterating works the following way:
 *
 * 1) Initially you call the function using a cursor (v) value of 0.
 * 2) The function performs one step of the iteration, and returns the
 *    new cursor value you must use in the next call.
 * 3) When the returned cursor is 0, the iteration is complete.
 *
 * The function guarantees all elements present in the
 * dictionary get returned between the start and end of the iteration.
 * However it is possible some elements get returned multiple times.
 *
 * For every element returned, the callback argument 'fn' is
 * called with 'privdata' as first argument and the dictionary entry
 * 'de' as second argument.
 *
 * HOW IT WORKS.
 *
 * The iteration algorithm was designed by Pieter Noordhuis.
 * The main idea is to increment a cursor starting from the higher order
 * bits. That is, instead of incrementing the cursor normally, the bits
 * of the cursor are reversed, then the cursor is incremented, and finally
 * the bits are reversed again.
 *
 * This strategy is needed because the hash table may be resized between
 * iteration calls.
 *
 * dict.c hash tables are always power of two in size, and they
 * use chaining, so the position of an element in a given table is given
 * by computing the bitwise AND between Hash(key) and SIZE-1
 * (where SIZE-1 is always the mask that is equivalent to taking the rest
 *  of the division between the Hash of the key and SIZE).
 *
 * For example if the current hash table size is 16, the mask is
 * (in binary) 1111. The position of a key in the hash table will always be
 * the last four bits of the hash output, and so forth.
 *
 * WHAT HAPPENS IF THE TABLE CHANGES IN SIZE?
 *
 * If the hash table grows, elements can go anywhere in one multiple of
 * the old bucket: for example let's say we already iterated with
 * a 4 bit cursor 1100 (the mask is 1111 because hash table size = 16).
 *
 * If the hash table will be resized to 64 elements, then the new mask will
 * be 111111. The new buckets you obtain by substituting in ??1100
 * with either 0 or 1 can be targeted only by keys we already visited
 * when scanning the bucket 1100 in the smaller hash table.
 *
 * By iterating the higher bits first, because of the inverted counter, the
 * cursor does not need to restart if the table size gets bigger. It will
 * continue iterating using cursors without '1100' at the end, and also
 * without any other combination of the final 4 bits already explored.
 *
 * Similarly when the table size shrinks over time, for example going from
 * 16 to 8, if a combination of the lower three bits (the mask for size 8
 * is 111) were already completely explored, it would not be visited again
 * because we are sure we tried, for example, both 0111 and 1111 (all the
 * variations of the higher bit) so we don't need to test it again.
 *
 * WAIT... YOU HAVE *TWO* TABLES DURING REHASHING!
 *
 * Yes, this is true, but we always iterate the smaller table first, then
 * we test all the expansions of the current cursor into the larger
 * table. For example if the current cursor is 101 and we also have a
 * larger table of size 16, we also test (0)101 and (1)101 inside the larger
 * table. This reduces the problem back to having only one table, where
 * the larger one, if it exists, is just an expansion of the smaller one.
 *
 * LIMITATIONS
 *
 * This iterator is completely stateless, and this is a huge advantage,
 * including no additional memory used.
 *
 * The disadvantages resulting from this design are:
 *
 * 1) It is possible we return elements more than once. However this is usually
 *    easy to deal with in the application level.
 * 2) The iterator must return multiple elements per call, as it needs to always
 *    return all the keys chained in a given bucket, and all the expansions, so
 *    we are sure we don't miss keys moving during rehashing.
 * 3) The reverse cursor is somewhat hard to understand at first, but this
 *    comment is supposed to help.
 */
//字典扫描
unsigned long dictScan(dict *d,
                       unsigned long v,
                       dictScanFunction *fn,
                       dictScanBucketFunction* bucketfn,
                       void *privdata)
{
    dictht *t0, *t1;
    const dictEntry *de, *next;
    unsigned long m0, m1;

    //如果字典为空, 则不处理
    if (dictSize(d) == 0) return 0;

    /* This is needed in case the scan callback tries to do dictFind or alike. */
    //如果字典正在rehash, 则停顿rehash
    dictPauseRehashing(d);

    //如果字典没有在rehash
    if (!dictIsRehashing(d)) {
        //获取hash表0的指针
        t0 = &(d->ht[0]);
        //获取hash表0的掩码
        m0 = t0->sizemask;

        /* Emit entries at cursor */
        //如果桶的回调函数存在, 则用回调函数处理要获取的桶
        if (bucketfn) bucketfn(privdata, &t0->table[v & m0]);
        //获取桶上的首节点
        de = t0->table[v & m0];
        //如果节点存在, 则遍历链表上的节点, 并且使用 fn 函数处理
        while (de) {
            next = de->next;
            fn(privdata, de);
            de = next;
        }

        /* Set unmasked bits so incrementing the reversed cursor
         * operates on the masked bits */
        //假如hash表0长度为8, 那么m0就应该为前29位为0, 后三位为1, 也就是 ...000111
        //~m0 也就是, ...111000, v |= ~m0 就相当于保留低位的数据, v最终结果为, 高29位为1, 低3位为实际数据, ...111xxx
        v |= ~m0;

        /* Increment the reverse cursor */
        //反转游标, 就变成 xxx111...111
        v = rev(v);
        //游标加1, 因为低位都是1, 加1之后, 就会进1, 最终相当于实际数据加1, 其实就相当于xx(x + 1)000...000
        v++;
        //再次返回转回原来的顺序
        v = rev(v);

    } else {
        //获取字段的hash表0的引用
        t0 = &d->ht[0];
        //获取字典的hash表1的引用
        t1 = &d->ht[1];

        /* Make sure t0 is the smaller and t1 is the bigger table */
        //判断那个hash表的容量最小, 小容量的hash表为t0
        if (t0->size > t1->size) {
            t0 = &d->ht[1];
            t1 = &d->ht[0];
        }

        //获取t0的掩码
        m0 = t0->sizemask;
        //获取t1的掩码
        m1 = t1->sizemask;

        /* Emit entries at cursor */
        //如果 bucketfn 函数不为null, 则使用bucketfn对链表进行处理
        if (bucketfn) bucketfn(privdata, &t0->table[v & m0]);
        //获取游标对应的首节点
        de = t0->table[v & m0];
        //遍历链表
        while (de) {
            //获取下一个节点
            next = de->next;
            //处理当前节点
            fn(privdata, de);
            de = next;
        }

        /* Iterate over indices in larger table that are the expansion
         * of the index pointed to by the cursor in the smaller table */
        //处理大hash表t1
        //小表的槽, 在按大表重hash后的槽都是相对固定的
        // 假如小表容量是8, 则他的槽二进制就是三位, 如: 001, 010等等, 我们以abc表示3位二进制变量
        // 当扩容到32, 则他们二进制位为5位, 如: 00010, 01010等, 我们以xxabc来表示5位后的二进制变量
        // 也就是扩容后, 落在二进制abc的值, 很有可能会重hash后会落在xxabc中,
        // 所以我们扫描小表的abc后, 再将abc作为后缀, 穷举xxabc中的xx, 就可以获取rehash两张表中原来在同一个槽的key值
        //如果是大表变小表同理
        do {
            /* Emit entries at cursor */
            //首先用桶函数处理
            if (bucketfn) bucketfn(privdata, &t1->table[v & m1]);
            //获取游标在大表t1对应的槽
            de = t1->table[v & m1];
            //遍历槽上的链表, 使用函数处理获得的节点
            while (de) {
                next = de->next;
                fn(privdata, de);
                de = next;
            }

            //为什么这里能直接往上递增呢?
            //假如是小表变大表, 上个游标xxabc的xx肯定是00, 所以在读大表时, 可以直接倒序往上加, 直到xx再次变00, 也就是穷举xx
            //假如是大表变小表, 上个游标xxabc的xx很可能不为00, 假如为01, 那么就代表着00和10是被访问过的了, 最终才会返回01的, 所以大表区动小表
            //可以参考 https://www.infoq.cn/article/piaabmlotbqcjkrt7l2v
            //以前是 v = (((v | m0) + 1) & ~m0) | (v & m0);   //BUG
            /* Increment the reverse cursor not covered by the smaller mask.*/
            //假如m1为...011111, ~m1就是...100000, v |= ~m1 就相当于 ...1xxabc
            v |= ~m1;
            //反转, 结果是 abcxx11...111
            v = rev(v);
            v++;
            v = rev(v);

            /* Continue while bits covered by mask difference is non-zero */
            //如果m0是...000111, m1是...011111, 那么 m0^m1就是...011000, 也就是只保留m1的高位
            //v & (m0 ^ m1) 就是, 当v相对于m0的高位都为0时, 退出循环
        } while (v & (m0 ^ m1));
    }

    //减少停顿rehash的状态
    dictResumeRehashing(d);

    return v;
}

/* ------------------------- private functions ------------------------------ */

/* Because we may need to allocate huge memory chunk at once when dict
 * expands, we will check this allocation is allowed or not if the dict
 * type has expandAllowed member function. */
//判断字典类型是否允许扩容
static int dictTypeExpandAllowed(dict *d) {
    //判断可否扩容函数如果为 NULL, 则返回 1
    if (d->type->expandAllowed == NULL) return 1;
    //调用字典类型的函数进行判断是否可以扩容
    //_dictNextPower(d->ht[0].used + 1) * sizeof(dictEntry*) 这个表示扩容后的内存
    //(double)d->ht[0].used / d->ht[0].size 这个表示字典当前的负载因子
    return d->type->expandAllowed(
                    _dictNextPower(d->ht[0].used + 1) * sizeof(dictEntry*),
                    (double)d->ht[0].used / d->ht[0].size);
}

/* Expand the hash table if needed */
//是否需要扩容hash表
static int _dictExpandIfNeeded(dict *d)
{
    /* Incremental rehashing already in progress. Return. */
    //如果字典正在扩容, 则返回 OK
    if (dictIsRehashing(d)) return DICT_OK;

    /* If the hash table is empty expand it to the initial size. */
    //如果hash字典数组是空, 则初始化
    if (d->ht[0].size == 0) return dictExpand(d, DICT_HT_INITIAL_SIZE);

    /* If we reached the 1:1 ratio, and we are allowed to resize the hash
     * table (global setting) or we should avoid it but the ratio between
     * elements/buckets is over the "safe" threshold, we resize doubling
     * the number of buckets. */
    //如果字典节点数大于hash数组长度
    if (d->ht[0].used >= d->ht[0].size &&
        //字典可以扩容
        (dict_can_resize ||
        //超过负载因子
         d->ht[0].used/d->ht[0].size > dict_force_resize_ratio) &&
         //字典类型的函数判断是否允许扩容
        dictTypeExpandAllowed(d))
    {
        //调用扩容方法, 扩容的容量为现有节点数 + 1
        return dictExpand(d, d->ht[0].used + 1);
    }
    return DICT_OK;
}

//保证hash数组的长度为 2^n 次方
/* Our hash table capability is a power of two */
static unsigned long _dictNextPower(unsigned long size)
{
    //从最小初始化长度开始
    unsigned long i = DICT_HT_INITIAL_SIZE;

    //最大扩张到 LONG_MAX + 1LU
    if (size >= LONG_MAX) return LONG_MAX + 1LU;
    //循环, 一直找到大于等于 size 的最小的 2^n 的数
    while(1) {
        if (i >= size)
            return i;
        i *= 2;
    }
}

/* Returns the index of a free slot that can be populated with
 * a hash entry for the given 'key'.
 * If the key already exists, -1 is returned
 * and the optional output parameter may be filled.
 *
 * Note that if we are in the process of rehashing the hash table, the
 * index is always returned in the context of the second (new) hash table. */
//返回 key 对应的槽索引
//dictEntry **existing 代表什么意思呢?  为了操作外面的dictEntry指针, 只能将 dictEntry 指针的指针传进来
//也就是 existing 代表 dictEntry指针的指针, *existing 表示dictEntry指针, 也就是 dictEntry *
static long _dictKeyIndex(dict *d, const void *key, uint64_t hash, dictEntry **existing)
{
    unsigned long idx, table;
    dictEntry *he;
    //如果传进来的 existing 指针不为 NULL, 设置 dictEntry 指针 dictEntry * 为 NULL
    if (existing) *existing = NULL;

    /* Expand the hash table if needed */
    //如果需要则扩容, 扩容失败则返回 -1
    if (_dictExpandIfNeeded(d) == DICT_ERR)
        return -1;
    //遍历hash表数组
    for (table = 0; table <= 1; table++) {
        //hash值与hash表的掩码计算出hash槽的索引
        idx = hash & d->ht[table].sizemask;
        /* Search if this slot does not already contain the given key */
        //获取槽上的链表
        he = d->ht[table].table[idx];
        while(he) {
            //如果传入的key等于节点上的key
            if (key==he->key || dictCompareKeys(d, key, he->key)) {
                //传进来的指针existing不为空, 则将存在的节点指针放入 *existing 指针中返回给外层使用
                if (existing) *existing = he;
                //返回 -1
                return -1;
            }
            //获取链表下一个节点
            he = he->next;
        }
        //如果不是在rehash, 则退出循环
        if (!dictIsRehashing(d)) break;
    }
    //返回hash槽索引
    return idx;
}

//清空字典
void dictEmpty(dict *d, void(callback)(void*)) {
    //清空hash表0
    _dictClear(d,&d->ht[0],callback);
    //清空hash表1
    _dictClear(d,&d->ht[1],callback);
    d->rehashidx = -1;
    d->pauserehash = 0;
}

//设置字典允许扩容
void dictEnableResize(void) {
    dict_can_resize = 1;
}

//设置字典不允许扩容
void dictDisableResize(void) {
    dict_can_resize = 0;
}

//获取key的hash值
uint64_t dictGetHash(dict *d, const void *key) {
    return dictHashKey(d, key);
}

/* Finds the dictEntry reference by using pointer and pre-calculated hash.
 * oldkey is a dead pointer and should not be accessed.
 * the hash value should be provided using dictGetHash.
 * no string / key comparison is performed.
 * return value is the reference to the dictEntry if found, or NULL if not found. */
dictEntry **dictFindEntryRefByPtrAndHash(dict *d, const void *oldptr, uint64_t hash) {
    dictEntry *he, **heref;
    unsigned long idx, table;

    if (dictSize(d) == 0) return NULL; /* dict is empty */
    for (table = 0; table <= 1; table++) {
        idx = hash & d->ht[table].sizemask;
        heref = &d->ht[table].table[idx];
        he = *heref;
        while(he) {
            if (oldptr==he->key)
                return heref;
            heref = &he->next;
            he = *heref;
        }
        if (!dictIsRehashing(d)) return NULL;
    }
    return NULL;
}

/* ------------------------------- Debugging ---------------------------------*/

#define DICT_STATS_VECTLEN 50
size_t _dictGetStatsHt(char *buf, size_t bufsize, dictht *ht, int tableid) {
    unsigned long i, slots = 0, chainlen, maxchainlen = 0;
    unsigned long totchainlen = 0;
    unsigned long clvector[DICT_STATS_VECTLEN];
    size_t l = 0;

    if (ht->used == 0) {
        return snprintf(buf,bufsize,
            "No stats available for empty dictionaries\n");
    }

    /* Compute stats. */
    for (i = 0; i < DICT_STATS_VECTLEN; i++) clvector[i] = 0;
    for (i = 0; i < ht->size; i++) {
        dictEntry *he;

        if (ht->table[i] == NULL) {
            clvector[0]++;
            continue;
        }
        slots++;
        /* For each hash entry on this slot... */
        chainlen = 0;
        he = ht->table[i];
        while(he) {
            chainlen++;
            he = he->next;
        }
        clvector[(chainlen < DICT_STATS_VECTLEN) ? chainlen : (DICT_STATS_VECTLEN-1)]++;
        if (chainlen > maxchainlen) maxchainlen = chainlen;
        totchainlen += chainlen;
    }

    /* Generate human readable stats. */
    l += snprintf(buf+l,bufsize-l,
        "Hash table %d stats (%s):\n"
        " table size: %lu\n"
        " number of elements: %lu\n"
        " different slots: %lu\n"
        " max chain length: %lu\n"
        " avg chain length (counted): %.02f\n"
        " avg chain length (computed): %.02f\n"
        " Chain length distribution:\n",
        tableid, (tableid == 0) ? "main hash table" : "rehashing target",
        ht->size, ht->used, slots, maxchainlen,
        (float)totchainlen/slots, (float)ht->used/slots);

    for (i = 0; i < DICT_STATS_VECTLEN-1; i++) {
        if (clvector[i] == 0) continue;
        if (l >= bufsize) break;
        l += snprintf(buf+l,bufsize-l,
            "   %s%ld: %ld (%.02f%%)\n",
            (i == DICT_STATS_VECTLEN-1)?">= ":"",
            i, clvector[i], ((float)clvector[i]/ht->size)*100);
    }

    /* Unlike snprintf(), return the number of characters actually written. */
    if (bufsize) buf[bufsize-1] = '\0';
    return strlen(buf);
}

void dictGetStats(char *buf, size_t bufsize, dict *d) {
    size_t l;
    char *orig_buf = buf;
    size_t orig_bufsize = bufsize;

    l = _dictGetStatsHt(buf,bufsize,&d->ht[0],0);
    buf += l;
    bufsize -= l;
    if (dictIsRehashing(d) && bufsize > 0) {
        _dictGetStatsHt(buf,bufsize,&d->ht[1],1);
    }
    /* Make sure there is a NULL term at the end. */
    if (orig_bufsize) orig_buf[orig_bufsize-1] = '\0';
}

/* ------------------------------- Benchmark ---------------------------------*/

#ifdef DICT_BENCHMARK_MAIN

#include "sds.h"

uint64_t hashCallback(const void *key) {
    return dictGenHashFunction((unsigned char*)key, sdslen((char*)key));
}

int compareCallback(void *privdata, const void *key1, const void *key2) {
    int l1,l2;
    DICT_NOTUSED(privdata);

    l1 = sdslen((sds)key1);
    l2 = sdslen((sds)key2);
    if (l1 != l2) return 0;
    return memcmp(key1, key2, l1) == 0;
}

void freeCallback(void *privdata, void *val) {
    DICT_NOTUSED(privdata);

    sdsfree(val);
}

dictType BenchmarkDictType = {
    hashCallback,
    NULL,
    NULL,
    compareCallback,
    freeCallback,
    NULL,
    NULL
};

#define start_benchmark() start = timeInMilliseconds()
#define end_benchmark(msg) do { \
    elapsed = timeInMilliseconds()-start; \
    printf(msg ": %ld items in %lld ms\n", count, elapsed); \
} while(0)

/* dict-benchmark [count] */
int main(int argc, char **argv) {
    long j;
    long long start, elapsed;
    dict *dict = dictCreate(&BenchmarkDictType,NULL);
    long count = 0;

    if (argc == 2) {
        count = strtol(argv[1],NULL,10);
    } else {
        count = 5000000;
    }

    start_benchmark();
    for (j = 0; j < count; j++) {
        int retval = dictAdd(dict,sdsfromlonglong(j),(void*)j);
        assert(retval == DICT_OK);
    }
    end_benchmark("Inserting");
    assert((long)dictSize(dict) == count);

    /* Wait for rehashing. */
    while (dictIsRehashing(dict)) {
        dictRehashMilliseconds(dict,100);
    }

    start_benchmark();
    for (j = 0; j < count; j++) {
        sds key = sdsfromlonglong(j);
        dictEntry *de = dictFind(dict,key);
        assert(de != NULL);
        sdsfree(key);
    }
    end_benchmark("Linear access of existing elements");

    start_benchmark();
    for (j = 0; j < count; j++) {
        sds key = sdsfromlonglong(j);
        dictEntry *de = dictFind(dict,key);
        assert(de != NULL);
        sdsfree(key);
    }
    end_benchmark("Linear access of existing elements (2nd round)");

    start_benchmark();
    for (j = 0; j < count; j++) {
        sds key = sdsfromlonglong(rand() % count);
        dictEntry *de = dictFind(dict,key);
        assert(de != NULL);
        sdsfree(key);
    }
    end_benchmark("Random access of existing elements");

    start_benchmark();
    for (j = 0; j < count; j++) {
        dictEntry *de = dictGetRandomKey(dict);
        assert(de != NULL);
    }
    end_benchmark("Accessing random keys");

    start_benchmark();
    for (j = 0; j < count; j++) {
        sds key = sdsfromlonglong(rand() % count);
        key[0] = 'X';
        dictEntry *de = dictFind(dict,key);
        assert(de == NULL);
        sdsfree(key);
    }
    end_benchmark("Accessing missing");

    start_benchmark();
    for (j = 0; j < count; j++) {
        sds key = sdsfromlonglong(j);
        int retval = dictDelete(dict,key);
        assert(retval == DICT_OK);
        key[0] += 17; /* Change first number to letter. */
        retval = dictAdd(dict,key,(void*)j);
        assert(retval == DICT_OK);
    }
    end_benchmark("Removing and adding");
}
#endif
