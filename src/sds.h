/* SDSLib 2.0 -- A C dynamic strings library
 *
 * Copyright (c) 2006-2015, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2015, Oran Agra
 * Copyright (c) 2015, Redis Labs, Inc
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

#ifndef __SDS_H
#define __SDS_H

// 1M = 1024 * 1024 * 1 byte
#define SDS_MAX_PREALLOC (1024*1024)
//用于标识, 创建sds时, 不初始化字符数组
extern const char *SDS_NOINIT;

#include <sys/types.h>
#include <stdarg.h>
#include <stdint.h>

//为字符数组定义别名, 为sds
typedef char *sds;

/* Note: sdshdr5 is never used, we just access the flags byte directly.
 * However is here to document the layout of type 5 SDS strings. */
//sds结构体从4.0开始, 开始使用这5种sdshdr${n}的定义, 用于更合理地分配内存, 注意, 只是定义了 sdshdr5, 但是不会使用.
// __attribute__ ((__packed__)) 是指定编译器属性, 非语言特性, packed属性的主要目的是让编译器更紧凑地使用内存, 具体可以google一下
//最长 2^5-1 长度的 sdshdr
struct __attribute__ ((__packed__)) sdshdr5 {
    //低三位保存类型标志, 高5位用于保存字符串长度. 最多能保存5bit长度的字符串
    unsigned char flags; /* 3 lsb of type, and 5 msb of string length */
    char buf[];
};
//最长 2^8-1 长度的 sdshdr
struct __attribute__ ((__packed__)) sdshdr8 {
    //len 表示已使用长度
    uint8_t len; /* used */
    //buf分配的总长度, 也就是数组的总大小, 剩余大小 = alloc - len
    uint8_t alloc; /* excluding the header and null terminator */
    //低3位保存类型标志
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    //字符数组
    char buf[];
};
//最长 2^16-1 长度的 sdshdr
struct __attribute__ ((__packed__)) sdshdr16 {
    uint16_t len; /* used */
    uint16_t alloc; /* excluding the header and null terminator */
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    char buf[];
};
//最长 2^32-1 长度的 sdshdr
struct __attribute__ ((__packed__)) sdshdr32 {
    uint32_t len; /* used */
    uint32_t alloc; /* excluding the header and null terminator */
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    char buf[];
};
//最长 2^64-1 长度的 sdshdr
struct __attribute__ ((__packed__)) sdshdr64 {
    uint64_t len; /* used */
    uint64_t alloc; /* excluding the header and null terminator */
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    char buf[];
};

//宏定义中, 用#于把宏参数变成一个字符串, 用##把两个宏参数粘合在一起

#define SDS_TYPE_5  0
#define SDS_TYPE_8  1
#define SDS_TYPE_16 2
#define SDS_TYPE_32 3
#define SDS_TYPE_64 4
//低三位的掩码,  也就是 00000111
#define SDS_TYPE_MASK 7
#define SDS_TYPE_BITS 3
//获取 sdshdr 引用地址, 并且将地址放到 sh 变量中
#define SDS_HDR_VAR(T,s) struct sdshdr##T *sh = (void*)((s)-(sizeof(struct sdshdr##T)));
//这里为什么 s - sizeof(struct sdshdr##T) 就能得到 sdshdr##T 呢
//可以看一下 sdshdr##T 的内存结构, 如: sdshdr8, 代表字符数组引用的sds在结构体的最后, 数组名不占空间, 我们可以得到
// 结构体的引用地址 = 字符数组的引用地址 - 结体的大小
//获取 sdshdr 引用地址
#define SDS_HDR(T,s) ((struct sdshdr##T *)((s)-(sizeof(struct sdshdr##T))))
//左移三位, 也就是获取高5位作为返回值
#define SDS_TYPE_5_LEN(f) ((f)>>SDS_TYPE_BITS)

//内联函数, 用于获取sds字符串的长度
static inline size_t sdslen(const sds s) {
    //获取数组下标外, 前一个char大小的值, 也就相当于 SDSHDR 的 flag
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        case SDS_TYPE_5:
            return SDS_TYPE_5_LEN(flags);
        case SDS_TYPE_8:
            return SDS_HDR(8,s)->len;
        case SDS_TYPE_16:
            return SDS_HDR(16,s)->len;
        case SDS_TYPE_32:
            return SDS_HDR(32,s)->len;
        case SDS_TYPE_64:
            return SDS_HDR(64,s)->len;
    }
    return 0;
}

//内联函数, 用于获取sds的剩余空间, 剩余空间 = alloc - len
static inline size_t sdsavail(const sds s) {
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        case SDS_TYPE_5: {
            return 0;
        }
        case SDS_TYPE_8: {
            SDS_HDR_VAR(8,s);
            return sh->alloc - sh->len;
        }
        case SDS_TYPE_16: {
            SDS_HDR_VAR(16,s);
            return sh->alloc - sh->len;
        }
        case SDS_TYPE_32: {
            SDS_HDR_VAR(32,s);
            return sh->alloc - sh->len;
        }
        case SDS_TYPE_64: {
            SDS_HDR_VAR(64,s);
            return sh->alloc - sh->len;
        }
    }
    return 0;
}

//设置sds的长度
static inline void sdssetlen(sds s, size_t newlen) {
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        case SDS_TYPE_5:
            {
                unsigned char *fp = ((unsigned char*)s)-1;
                *fp = SDS_TYPE_5 | (newlen << SDS_TYPE_BITS);
            }
            break;
        case SDS_TYPE_8:
            SDS_HDR(8,s)->len = newlen;
            break;
        case SDS_TYPE_16:
            SDS_HDR(16,s)->len = newlen;
            break;
        case SDS_TYPE_32:
            SDS_HDR(32,s)->len = newlen;
            break;
        case SDS_TYPE_64:
            SDS_HDR(64,s)->len = newlen;
            break;
    }
}

//扩展sds的长度
static inline void sdsinclen(sds s, size_t inc) {
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        case SDS_TYPE_5:
            {
                unsigned char *fp = ((unsigned char*)s)-1;
                unsigned char newlen = SDS_TYPE_5_LEN(flags)+inc;
                *fp = SDS_TYPE_5 | (newlen << SDS_TYPE_BITS);
            }
            break;
        case SDS_TYPE_8:
            SDS_HDR(8,s)->len += inc;
            break;
        case SDS_TYPE_16:
            SDS_HDR(16,s)->len += inc;
            break;
        case SDS_TYPE_32:
            SDS_HDR(32,s)->len += inc;
            break;
        case SDS_TYPE_64:
            SDS_HDR(64,s)->len += inc;
            break;
    }
}

//获取数组分配的大小
/* sdsalloc() = sdsavail() + sdslen() */
static inline size_t sdsalloc(const sds s) {
    //获取 flag 标记
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        case SDS_TYPE_5:
            return SDS_TYPE_5_LEN(flags);
        case SDS_TYPE_8:
            return SDS_HDR(8,s)->alloc;
        case SDS_TYPE_16:
            return SDS_HDR(16,s)->alloc;
        case SDS_TYPE_32:
            return SDS_HDR(32,s)->alloc;
        case SDS_TYPE_64:
            return SDS_HDR(64,s)->alloc;
    }
    return 0;
}

//重置sds已分配容量的大小
static inline void sdssetalloc(sds s, size_t newlen) {
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        case SDS_TYPE_5:
            /* Nothing to do, this type has no total allocation info. */
            break;
        case SDS_TYPE_8:
            SDS_HDR(8,s)->alloc = newlen;
            break;
        case SDS_TYPE_16:
            SDS_HDR(16,s)->alloc = newlen;
            break;
        case SDS_TYPE_32:
            SDS_HDR(32,s)->alloc = newlen;
            break;
        case SDS_TYPE_64:
            SDS_HDR(64,s)->alloc = newlen;
            break;
    }
}

//创建给定长度的 sds
sds sdsnewlen(const void *init, size_t initlen);
sds sdstrynewlen(const void *init, size_t initlen);
//创建字符串数组创建 sds
sds sdsnew(const char *init);
//创建空在的 sds
sds sdsempty(void);
//复制sds对象返回
sds sdsdup(const sds s);
//释放sds
void sdsfree(sds s);
//用空字符串扩展sds的长度
sds sdsgrowzero(sds s, size_t len);
//将二进制安全的字符串附加到现有的 buf 数组之后
sds sdscatlen(sds s, const void *t, size_t len);
//sds拼接字符串
sds sdscat(sds s, const char *t);
//拼接两sds
sds sdscatsds(sds s, const sds t);
//丢弃 sds 字符数组中的原内容，将长为 len 的字符串拷贝至 sds 的 buf 中
sds sdscpylen(sds s, const char *t, size_t len);
//将给定的C字符串复制到sds里面, 覆盖SDS原有的字符串
sds sdscpy(sds s, const char *t);

//将格式化字符串拼接到 sds 之后
sds sdscatvprintf(sds s, const char *fmt, va_list ap);
#ifdef __GNUC__
sds sdscatprintf(sds s, const char *fmt, ...)
    __attribute__((format(printf, 2, 3)));
#else
sds sdscatprintf(sds s, const char *fmt, ...);
#endif

sds sdscatfmt(sds s, char const *fmt, ...);
//接受一个SDS和一个字符串作为参数, 从SDS中移除所有在D字符串中出现过的字符
sds sdstrim(sds s, const char *cset);
//依据 start 和 end 索引下标修剪 sds 字符串
void sdsrange(sds s, ssize_t start, ssize_t end);
//更新sds长度
void sdsupdatelen(sds s);
//清空sds内空
void sdsclear(sds s);
//对比两个sds是否相同
int sdscmp(const sds s1, const sds s2);
//使用长为 seplen 的二进制安全字符串 sep 作为分隔符，将长为 len 的二进制安全字符串 s 分割成 count 个 sds 字符串
sds *sdssplitlen(const char *s, ssize_t len, const char *sep, int seplen, int *count);
//释放 sdssplitlen 生成的动态数组的内存
void sdsfreesplitres(sds *tokens, int count);
//字符串变小写
void sdstolower(sds s);
//字符串变大小
void sdstoupper(sds s);
//将long long 值转成 sds
sds sdsfromlonglong(long long value);
//处理特殊字符, 非打印字符会转成16进制打印, 相当于将字符串变成可打印的
sds sdscatrepr(sds s, const char *p, size_t len);
//解析命令行参数, 返回sds数组和参数个数
sds *sdssplitargs(const char *line, int *argc);
//遍历 sds 字符串，将在字符串 from 中出现的字符替换成 to 中对应位置的字符
sds sdsmapchars(sds s, const char *from, const char *to, size_t setlen);
//使用 C 风格字符串 sep 作为分隔符，将 C 风格字符串数组拼接为一个 sds
sds sdsjoin(char **argv, int argc, char *sep);
//以 sep 字符串为分割符, 将sds数组拼接成字符串
sds sdsjoinsds(sds *argv, int argc, const char *sep, size_t seplen);

/* Callback for sdstemplate. The function gets called by sdstemplate
 * every time a variable needs to be expanded. The variable name is
 * provided as variable, and the callback is expected to return a
 * substitution value. Returning a NULL indicates an error.
 */
//模版中变量的处理函数指针
typedef sds (*sdstemplate_callback_t)(const sds variable, void *arg);
//处理字符串模板, 如: "my name is {1}"
sds sdstemplate(const char *template, sdstemplate_callback_t cb_func, void *cb_arg);

/* Low level functions exposed to the user API */
//空间预分配, 减少内存重分配次数
sds sdsMakeRoomFor(sds s, size_t addlen);
//sds增加长度
void sdsIncrLen(sds s, ssize_t incr);
//回收sds空闲的内存空间
sds sdsRemoveFreeSpace(sds s);
//获取sds分配的空间大小
size_t sdsAllocSize(sds s);
//获取sds分配内存的指针, 也就是sdsHdr对象的指针
void *sdsAllocPtr(sds s);

/* Export the allocator used by SDS to the program using SDS.
 * Sometimes the program SDS is linked to, may use a different set of
 * allocators, but may want to allocate or free things that SDS will
 * respectively free or allocate. */
//分配sds内存
void *sds_malloc(size_t size);
//重分配sds内存
void *sds_realloc(void *ptr, size_t size);
//释放sds内存
void sds_free(void *ptr);

#ifdef REDIS_TEST
int sdsTest(int argc, char *argv[]);
#endif

#endif
