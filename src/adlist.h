/* adlist.h - A generic doubly linked list implementation
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

#ifndef __ADLIST_H__
#define __ADLIST_H__

/* Node, List, and Iterator are the only data structures used currently. */

//链表节点的结构体
typedef struct listNode {
    //前一个节点的引用
    struct listNode *prev;
    //下一个节点的引用
    struct listNode *next;
    //当前节点的值, 相当于 Object 类型
    void *value;
} listNode;

//链表迭代器
typedef struct listIter {
    //当前遍历节点
    listNode *next;
    //迭代方向, 可以向前或者向后
    int direction;
} listIter;

//链表结构体
typedef struct list {
    //链表首节点
    listNode *head;
    //链表尾节点
    listNode *tail;
    //结构体的函数相当于多态, 由具体的对象提供对应的函数实现
    //链表复制指定节点的value的值, 值如果是结构体, 则需要对应的函数来拷贝
    void *(*dup)(void *ptr);
    //链表释放指定节点的value的内存的函数, 值如果是结构体, 则需要对应的函数释放内存
    void (*free)(void *ptr);
    //值的比较函数, 相当于 java 的equals, 用于比较结构体是否相等
    int (*match)(void *ptr, void *key);
    //链表长度
    unsigned long len;
} list;

/* Functions implemented as macros */
//获取链表长度
#define listLength(l) ((l)->len)
//获取链表第一个节点
#define listFirst(l) ((l)->head)
//获取链表最后一个节点
#define listLast(l) ((l)->tail)
//获取链表前一个节点
#define listPrevNode(n) ((n)->prev)
//获取链表后一个节点
#define listNextNode(n) ((n)->next)
//获取链表节点的值
#define listNodeValue(n) ((n)->value)

//设置链表的复制函数
#define listSetDupMethod(l,m) ((l)->dup = (m))
//设置链表结构体的释放函数
#define listSetFreeMethod(l,m) ((l)->free = (m))
//设置链表结构体的匹配函数
#define listSetMatchMethod(l,m) ((l)->match = (m))

//获取链表的复制函数
#define listGetDupMethod(l) ((l)->dup)
//获取链表的释放函数
#define listGetFreeMethod(l) ((l)->free)
//获取链表的匹配函数
#define listGetMatchMethod(l) ((l)->match)

/* Prototypes */
//链表创建函数
list *listCreate(void);
//链表释放
void listRelease(list *list);
//清空链表
void listEmpty(list *list);
//添加节点到链表头
list *listAddNodeHead(list *list, void *value);
//添加节点到链表尾
list *listAddNodeTail(list *list, void *value);

//根据原有节点指定位置插入新节点
list *listInsertNode(list *list, listNode *old_node, void *value, int after);
//删除指定节点
void listDelNode(list *list, listNode *node);
//获取节点迭代器
listIter *listGetIterator(list *list, int direction);
//获取迭代器下一个节点
listNode *listNext(listIter *iter);
//释放迭代器
void listReleaseIterator(listIter *iter);
//复制链表
list *listDup(list *orig);
//根据 key 值查找节点
listNode *listSearchKey(list *list, void *key);
//根据索引获取节点
listNode *listIndex(list *list, long index);
//设置迭代器, 指定链表从head开始遍历
void listRewind(list *list, listIter *li);
//设置迭代器, 指定链表从tail开始遍历
void listRewindTail(list *list, listIter *li);
//将链表尾节点变头节点
void listRotateTailToHead(list *list);
//将链表头节点变尾节点
void listRotateHeadToTail(list *list);
//将链表o拼接到链表l后面
void listJoin(list *l, list *o);

/* Directions for iterators */
//前面开始遍历
#define AL_START_HEAD 0
//后面开始遍历
#define AL_START_TAIL 1

#endif /* __ADLIST_H__ */
