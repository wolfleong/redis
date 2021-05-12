/* zmalloc - total amount of allocated memory aware version of malloc()
 *
 * Copyright (c) 2009-2010, Salvatore Sanfilippo <antirez at gmail dot com>
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

/*
Redis内存模型:

|--------|:代表8个字节大小


| 头部   |            实际内存              |
|--------|--------|--------|--------|--------|
         ^
         |
         ptr:实际返回的地址

上面的内存模型说明了两个问题：

1.内存对齐,都是8个字节的，可以提高cpu响应速度

2.返回的实际地址不包括头部

ps:从左到右内存地址是增加的,默认大端模式
*/

#ifndef __ZMALLOC_H
#define __ZMALLOC_H

/* Double expansion needed for stringification of macro values. */
#define __xstr(s) __str(s)
//将s变成字符串
#define __str(s) #s
//分别判断使用tcmalloc库/jemalloc库/苹果库哪个作为底层的malloc函数调用
#if defined(USE_TCMALLOC)
//拼接 ZMALLOC_LIB 字符串
#define ZMALLOC_LIB ("tcmalloc-" __xstr(TC_VERSION_MAJOR) "." __xstr(TC_VERSION_MINOR))
//引入库
#include <google/tcmalloc.h>
//限定使用的版本号
#if (TC_VERSION_MAJOR == 1 && TC_VERSION_MINOR >= 6) || (TC_VERSION_MAJOR > 1)
//定义 HAVE_MALLOC_SIZE
#define HAVE_MALLOC_SIZE 1
//定义获取指针对应的内存大小
#define zmalloc_size(p) tc_malloc_size(p)
#else
#error "Newer version of tcmalloc required"
#endif

#elif defined(USE_JEMALLOC)
//拼接ZMALLOC_LIB字符串
#define ZMALLOC_LIB ("jemalloc-" __xstr(JEMALLOC_VERSION_MAJOR) "." __xstr(JEMALLOC_VERSION_MINOR) "." __xstr(JEMALLOC_VERSION_BUGFIX))
//引 jemalloc 的库
#include <jemalloc/jemalloc.h>
//限定版本号
#if (JEMALLOC_VERSION_MAJOR == 2 && JEMALLOC_VERSION_MINOR >= 1) || (JEMALLOC_VERSION_MAJOR > 2)
#define HAVE_MALLOC_SIZE 1
#define zmalloc_size(p) je_malloc_usable_size(p)
#else
#error "Newer version of jemalloc required"
#endif

//mac的库
#elif defined(__APPLE__)
#include <malloc/malloc.h>
//是否存在获取已分配内存大小的方法
#define HAVE_MALLOC_SIZE 1
//获取指针对象分配内存的大小, 为什么需要这个方法呢, zmalloc_size 会返回不包括内存大小头(PREFIX_SIZE)的内存大小
#define zmalloc_size(p) malloc_size(p)
#endif

/* On native libc implementations, we should still do our best to provide a
 * HAVE_MALLOC_SIZE capability. This can be set explicitly as well:
 *
 * NO_MALLOC_USABLE_SIZE disables it on all platforms, even if they are
 *      known to support it.
 * USE_MALLOC_USABLE_SIZE forces use of malloc_usable_size() regardless
 *      of platform.
 */
//没有声明内存分配的库
#ifndef ZMALLOC_LIB
//定义ZMALLOC_LIB为"libc"
#define ZMALLOC_LIB "libc"
//如果存在 malloc_usable_size() 方法
//malloc_usable_size 是glibc中malloc.h的函数, 这个函数中传入一个指针，返回指针指向的空间实际占用的大小,
// 这个返回的大小，可能会比使用malloc申请的要大，由于系统的内存对齐或者最小分配限制
#if !defined(NO_MALLOC_USABLE_SIZE) && \
    (defined(__GLIBC__) || defined(__FreeBSD__) || \
     defined(USE_MALLOC_USABLE_SIZE))
#include <malloc.h>
#define HAVE_MALLOC_SIZE 1
#define zmalloc_size(p) malloc_usable_size(p)
#endif
#endif

/* We can enable the Redis defrag capabilities only if we are using Jemalloc
 * and the version used is our special version modified for Redis having
 * the ability to return per-allocation fragmentation hints. */
#if defined(USE_JEMALLOC) && defined(JEMALLOC_FRAG_HINT)
//定义是否支持内存碎片整理
#define HAVE_DEFRAG
#endif

//申请大小为size的内存空间, 不进行初始化, 有可能有脏数据
void *zmalloc(size_t size);
//以块的形式申请内存, 默认是1块, 对应 calloc, 并初始化为0
void *zcalloc(size_t size);
//重新调用已申请的内存大小为size
void *zrealloc(void *ptr, size_t size);
void *ztrymalloc(size_t size);
void *ztrycalloc(size_t size);
void *ztryrealloc(void *ptr, size_t size);
//释放内存
void zfree(void *ptr);
void *zmalloc_usable(size_t size, size_t *usable);
void *zcalloc_usable(size_t size, size_t *usable);
void *zrealloc_usable(void *ptr, size_t size, size_t *usable);
void *ztrymalloc_usable(size_t size, size_t *usable);
void *ztrycalloc_usable(size_t size, size_t *usable);
void *ztryrealloc_usable(void *ptr, size_t size, size_t *usable);
void zfree_usable(void *ptr, size_t *usable);
//字符串复制
char *zstrdup(const char *s);
//获取redis已经使用(分配)的内存大小
size_t zmalloc_used_memory(void);
//自定义内存溢出时回调函数
void zmalloc_set_oom_handler(void (*oom_handler)(size_t));
//获取RSS(常驻内存集)大小
size_t zmalloc_get_rss(void);
int zmalloc_get_allocator_info(size_t *allocated, size_t *active, size_t *resident);
void set_jemalloc_bg_thread(int enable);
int jemalloc_purge();
//获取进程私有的内容已经发生更改的内存大小
size_t zmalloc_get_private_dirty(long pid);
size_t zmalloc_get_smap_bytes_by_field(char *field, long pid);
//获取物理内存大小
size_t zmalloc_get_memory_size(void);
//直接调用系统free函数释放已分配的内存
void zlibc_free(void *ptr);

//如果开启内存碎片整理
#ifdef HAVE_DEFRAG
void zfree_no_tcache(void *ptr);
void *zmalloc_no_tcache(size_t size);
#endif

//没有获取已分配内存大小的方法, 则声明两个函数, 给 zmalloc.c 进行手动实现, 这里有点像java的抽象方法
#ifndef HAVE_MALLOC_SIZE
size_t zmalloc_size(void *ptr);
size_t zmalloc_usable_size(void *ptr);
#else
//将 zmalloc_size 方法重定义为 zmalloc_usable_size, 用于获取指针对象大小
#define zmalloc_usable_size(p) zmalloc_size(p)
#endif

#ifdef REDIS_TEST
int zmalloc_test(int argc, char **argv);
#endif

#endif /* __ZMALLOC_H */
