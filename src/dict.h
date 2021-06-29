/* Hash Tables Implementation.
 *
 * This file implements in-memory hash tables with insert/del/replace/find/
 * get-random-element operations. Hash tables will auto-resize if needed
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

#ifndef __DICT_H
#define __DICT_H

#include "mt19937-64.h"
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>

#define DICT_OK 0
#define DICT_ERR 1

/* Unused arguments generate annoying warnings... */
#define DICT_NOTUSED(V) ((void) V)

//字典的节点
typedef struct dictEntry {
    //节点的key
    void *key;
    //节点的值 v , v 可以是指针, 也可以是uint64整数, 也可以是int64整数, 还可以是浮点数
    union {
        void *val;
        uint64_t u64;
        int64_t s64;
        double d;
    } v;
    //下一个节点
    struct dictEntry *next;
} dictEntry;

typedef struct dictType {
    //键的hash函数
    uint64_t (*hashFunction)(const void *key);
    //复制键的函数
    void *(*keyDup)(void *privdata, const void *key);
    //值的复制函数
    void *(*valDup)(void *privdata, const void *obj);
    //键的比较函数
    int (*keyCompare)(void *privdata, const void *key1, const void *key2);
    //键的销毁函数
    void (*keyDestructor)(void *privdata, void *key);
    //值的销毁函数
    void (*valDestructor)(void *privdata, void *obj);
    //根据扩容后的数组的内存和负载因子判断是否可以扩容
    int (*expandAllowed)(size_t moreMem, double usedRatio);
} dictType;

/* This is our hash table structure. Every dictionary has two of this as we
 * implement incremental rehashing, for the old to the new table. */
//hash字典结构体
typedef struct dictht {
    //hash表的指针数组, dictEntry * 表示hash节点的指针, 再加一个 *, 也就是dictEntry ** 表示数组的首地址
    dictEntry **table;
    //hash数组大小, 一般为 2^n
    unsigned long size;
    //hash数组长度掩码, sizemask =  size - 1
    unsigned long sizemask;
    //hash字典的kv对的个数
    unsigned long used;
} dictht;

//字典的结构体
typedef struct dict {
    //字段类型的指针
    dictType *type;
    //携带的私有数据
    void *privdata;
    //hash字典的数组, 长度为2, 主要是用于渐式hash时使用
    dictht ht[2];
    //rehash下一个要迁移的桶索引, rehash 不进行时为 -1
    long rehashidx; /* rehashing not in progress if rehashidx == -1 */
    // pauserehash > 0 表示 rehash 是暂停的. 安全的迭代需要停止
    int16_t pauserehash; /* If >0 rehashing is paused (<0 indicates coding error) */
} dict;

/* If safe is set to 1 this is a safe iterator, that means, you can call
 * dictAdd, dictFind, and other functions against the dictionary even while
 * iterating. Otherwise it is a non safe iterator, and only dictNext()
 * should be called while iterating. */
//hash字段的迭代器
typedef struct dictIterator {
    //字典的指针
    dict *d;
    //hash槽索引下标
    long index;
    //table 迭代中的hash表数组ht的索引, safe 表示是否安全
    int table, safe;
    //entry表示当前已返回的节点, nextEntry表示下一个节点
    dictEntry *entry, *nextEntry;
    /* unsafe iterator fingerprint for misuse detection. */
    //字典当前状态的签名, 64位的hash值
    long long fingerprint;
} dictIterator;

typedef void (dictScanFunction)(void *privdata, const dictEntry *de);
typedef void (dictScanBucketFunction)(void *privdata, dictEntry **bucketref);

/* This is the initial size of every hash table */
//hash表最小初始化大小
#define DICT_HT_INITIAL_SIZE     4

/* ------------------------------- Macros ------------------------------------*/
//释放val值的内存. 如果val的释放函数存在, 则调用函数进行释放
#define dictFreeVal(d, entry) \
    if ((d)->type->valDestructor) \
        (d)->type->valDestructor((d)->privdata, (entry)->v.val)

//设置一个值到 entry 中. 如果有复制函数, 则用复制函数复制一个值进行赋值, 否则直接赋值
#define dictSetVal(d, entry, _val_) do { \
    if ((d)->type->valDup) \
        (entry)->v.val = (d)->type->valDup((d)->privdata, _val_); \
    else \
        (entry)->v.val = (_val_); \
} while(0)

//回收u_int64类型val的内存
#define dictSetSignedIntegerVal(entry, _val_) \
    do { (entry)->v.s64 = _val_; } while(0)

//回收u_int64类型val的内存
#define dictSetUnsignedIntegerVal(entry, _val_) \
    do { (entry)->v.u64 = _val_; } while(0)

//回收double类型val的内存
#define dictSetDoubleVal(entry, _val_) \
    do { (entry)->v.d = _val_; } while(0)

//回收key的内存
#define dictFreeKey(d, entry) \
    if ((d)->type->keyDestructor) \
        (d)->type->keyDestructor((d)->privdata, (entry)->key)

//设置key到 dictEntry 中. 如果有复制函数, 则复制key再赋值, 否则直接赋值
#define dictSetKey(d, entry, _key_) do { \
    if ((d)->type->keyDup) \
        (entry)->key = (d)->type->keyDup((d)->privdata, _key_); \
    else \
        (entry)->key = (_key_); \
} while(0)

//比较key是否相等, 如果key比较函数存在, 则用函数比较, 否则使用 == 比较
#define dictCompareKeys(d, key1, key2) \
    (((d)->type->keyCompare) ? \
        (d)->type->keyCompare((d)->privdata, key1, key2) : \
        (key1) == (key2))

//计算字典的键的hash值
#define dictHashKey(d, key) (d)->type->hashFunction(key)
//获取节点的键
#define dictGetKey(he) ((he)->key)
//获取节点值
#define dictGetVal(he) ((he)->v.val)
//获取节点的 int_64 的值
#define dictGetSignedIntegerVal(he) ((he)->v.s64)
//获取节点的u_int64的值
#define dictGetUnsignedIntegerVal(he) ((he)->v.u64)
//获取节点的double类型的值
#define dictGetDoubleVal(he) ((he)->v.d)
//获取hash数组的总大小(包括rehash数组)
#define dictSlots(d) ((d)->ht[0].size+(d)->ht[1].size)
//获取点节的数量
#define dictSize(d) ((d)->ht[0].used+(d)->ht[1].used)
//判断字段是否在 rehash 中
#define dictIsRehashing(d) ((d)->rehashidx != -1)
//字典rehash设置停顿. 为什么能一直加呢, 因为可能有多个安全的迭代器正在迭代
#define dictPauseRehashing(d) (d)->pauserehash++
//字段rehash设置取消停顿
#define dictResumeRehashing(d) (d)->pauserehash--

/* If our unsigned long type can store a 64 bit number, use a 64 bit PRNG. */
//定义随机函数
#if ULONG_MAX >= 0xffffffffffffffff
#define randomULong() ((unsigned long) genrand64_int64())
#else
#define randomULong() random()
#endif

/* API */
//创建字典, hash数组是空的
dict *dictCreate(dictType *type, void *privDataPtr);
//扩容字典, 如果hash数组没创建, 则进行创建, 创建失败则报内存溢出
int dictExpand(dict *d, unsigned long size);
//尝试扩容, 扩容失败则返回错误
int dictTryExpand(dict *d, unsigned long size);
//字典添加kv
int dictAdd(dict *d, void *key, void *val);
//根据key创建新的节点, 如果存在则返回 NULL
dictEntry *dictAddRaw(dict *d, void *key, dictEntry **existing);
//根据key添加一个节点, 如果存在则直接返回已存在的
dictEntry *dictAddOrFind(dict *d, void *key);
//如果节点不存在, 则添加, 如果节点存在, 则覆盖值
int dictReplace(dict *d, void *key, void *val);
//删除key
int dictDelete(dict *d, const void *key);
dictEntry *dictUnlink(dict *ht, const void *key);
void dictFreeUnlinkedEntry(dict *d, dictEntry *he);
//回收字典内存
void dictRelease(dict *d);
//根据key获取字典中的节点
dictEntry * dictFind(dict *d, const void *key);
//获取key对应的值
void *dictFetchValue(dict *d, const void *key);
//字典表调整大小, 以字典节点数量作为hash表的大小
int dictResize(dict *d);
//获取字典迭代器
dictIterator *dictGetIterator(dict *d);
dictIterator *dictGetSafeIterator(dict *d);
dictEntry *dictNext(dictIterator *iter);
void dictReleaseIterator(dictIterator *iter);
dictEntry *dictGetRandomKey(dict *d);
dictEntry *dictGetFairRandomKey(dict *d);
unsigned int dictGetSomeKeys(dict *d, dictEntry **des, unsigned int count);
void dictGetStats(char *buf, size_t bufsize, dict *d);
uint64_t dictGenHashFunction(const void *key, int len);
uint64_t dictGenCaseHashFunction(const unsigned char *buf, int len);
void dictEmpty(dict *d, void(callback)(void*));
void dictEnableResize(void);
void dictDisableResize(void);
//字典rehash
int dictRehash(dict *d, int n);
int dictRehashMilliseconds(dict *d, int ms);
void dictSetHashFunctionSeed(uint8_t *seed);
uint8_t *dictGetHashFunctionSeed(void);
unsigned long dictScan(dict *d, unsigned long v, dictScanFunction *fn, dictScanBucketFunction *bucketfn, void *privdata);
//获取key的hash值
uint64_t dictGetHash(dict *d, const void *key);
dictEntry **dictFindEntryRefByPtrAndHash(dict *d, const void *oldptr, uint64_t hash);

/* Hash table types */
extern dictType dictTypeHeapStringCopyKey;
extern dictType dictTypeHeapStrings;
extern dictType dictTypeHeapStringCopyKeyValue;

#endif /* __DICT_H */
