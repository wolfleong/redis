/* adlist.c - A generic doubly linked list implementation
 *
 * Copyright (c) 2006-2010, Salvatore Sanfilippo <antirez at gmail dot com>
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


#include <stdlib.h>
#include "adlist.h"
#include "zmalloc.h"

/* Create a new list. The created list can be freed with
 * listRelease(), but private value of every node need to be freed
 * by the user before to call listRelease(), or by setting a free method using
 * listSetFreeMethod.
 *
 * On error, NULL is returned. Otherwise the pointer to the new list. */
//创建空链表
list *listCreate(void)
{
    //声明链表
    struct list *list;

    //分配链表内存
    if ((list = zmalloc(sizeof(*list))) == NULL)
        return NULL;
    //初始化首节点与尾节点都为空
    list->head = list->tail = NULL;
    //设置长度为 0
    list->len = 0;
    list->dup = NULL;
    list->free = NULL;
    list->match = NULL;
    return list;
}

/* Remove all the elements from the list without destroying the list itself. */
//清空链表元素, 不回收链表对象的内存
void listEmpty(list *list)
{
    //链表当前长度
    unsigned long len;
    //current 当前遍历指针
    //next 下一个节点的指针
    listNode *current, *next;

    //获取首节点
    current = list->head;
    //获取链表长度
    len = list->len;
    //按长度遍历
    while(len--) {
        //获取下一个节点
        next = current->next;
        //如果有指定释放内存的函数, 则调用函数释放节点的值
        if (list->free) list->free(current->value);
        //释放节点内存
        zfree(current);
        //当前指针替换成下一个节点
        current = next;
    }
    //首节点和尾节点置 NULL
    list->head = list->tail = NULL;
    //长度设置为 0
    list->len = 0;
}

/* Free the whole list.
 *
 * This function can't fail. */
//释放链表内存
void listRelease(list *list)
{
    //清空链表
    listEmpty(list);
    //释放链表内存
    zfree(list);
}

/* Add a new node to the list, to head, containing the specified 'value'
 * pointer as value.
 *
 * On error, NULL is returned and no operation is performed (i.e. the
 * list remains unaltered).
 * On success the 'list' pointer you pass to the function is returned. */
//添加节点到链表头
list *listAddNodeHead(list *list, void *value)
{
    //节点指针
    listNode *node;

    //分配节点内存, 分配不了则直接返回 NULL
    if ((node = zmalloc(sizeof(*node))) == NULL)
        return NULL;
    //设置节点的值
    node->value = value;
    //如果链表长度为0 , 则表示链表为空, 直接指定首节点和尾节点
    if (list->len == 0) {
        list->head = list->tail = node;
        node->prev = node->next = NULL;
    } else {
        //设置当前节点为首节点, 并且加入链表中
        node->prev = NULL;
        node->next = list->head;
        list->head->prev = node;
        list->head = node;
    }
    //链表长度加 1
    list->len++;
    //返回链表
    return list;
}

/* Add a new node to the list, to tail, containing the specified 'value'
 * pointer as value.
 *
 * On error, NULL is returned and no operation is performed (i.e. the
 * list remains unaltered).
 * On success the 'list' pointer you pass to the function is returned. */
//添加节点到链表尾
list *listAddNodeTail(list *list, void *value)
{
    //声明节点
    listNode *node;

    //分配节点内存, 分配不成功, 则返回 NULL
    if ((node = zmalloc(sizeof(*node))) == NULL)
        return NULL;
    //设置节点的值
    node->value = value;
    //如果链表为空
    if (list->len == 0) {
        //初始化链表首节点和尾节点都为当前节点
        list->head = list->tail = node;
        //当前节点的前一个节点和后一个节点都为 NULL
        node->prev = node->next = NULL;
    } else {
        //链表不为空
        //设置原尾节点为当前节点的前一个节点
        node->prev = list->tail;
        //设置尾节点为 NULL
        node->next = NULL;
        //链表原尾节点的下一个节点为当前节点
        list->tail->next = node;
        //更新链表的为节点为当前节点
        list->tail = node;
    }
    //节点长度加 1
    list->len++;
    //返回链表指针
    return list;
}

//根据原有节点指定位置插入新节点
list *listInsertNode(list *list, listNode *old_node, void *value, int after) {
    //声明新的节点
    listNode *node;

    //新节点分配内存
    if ((node = zmalloc(sizeof(*node))) == NULL)
        return NULL;
    //设置新节点的值
    node->value = value;
    //如果插入old_node的后面
    if (after) {
        //新节点的上一个节点设置为旧节点
        node->prev = old_node;
        //新节点的下一个节点设置为旧节点的下一个节点
        node->next = old_node->next;
        //如果旧节点是尾节点
        if (list->tail == old_node) {
            //重置当前节点为尾节点
            list->tail = node;
        }
    } else {
        //如果插入old_node的前面
        //新节点的下一个节点为旧节点
        node->next = old_node;
        //新节点的前一个节点为旧节点的前一个节点
        node->prev = old_node->prev;
        //如果旧节点为首节点
        if (list->head == old_node) {
            //更新链表首节点为新的节点
            list->head = node;
        }
    }
    //更新前面节点的下一个节点为 当前节点
    if (node->prev != NULL) {
        node->prev->next = node;
    }
    //更新后面节点的上一个节点为 当前节点
    if (node->next != NULL) {
        node->next->prev = node;
    }
    //增加链表长度
    list->len++;
    //返回
    return list;
}

/* Remove the specified node from the specified list.
 * It's up to the caller to free the private value of the node.
 *
 * This function can't fail. */
//删除节点, 主要是各种链表操作哈
void listDelNode(list *list, listNode *node)
{
    //如果当前节点的前一个节点不为 NULL
    if (node->prev)
        //设置前一个节点的下一个节点为当前节点的下一个节点
        node->prev->next = node->next;
    else
        //要删除的节点为首节点, 直接将head设置为下一个节点
        list->head = node->next;
    //如果要删除的节点的下一个节点不为 NULL
    if (node->next)
        //将要删除节点的下一个节点的前一个节点, 重置为当前节点的前一个节点
        node->next->prev = node->prev;
    else
        //要删除的节点是尾节点, 设置前一个节点为尾节点
        list->tail = node->prev;
    //如果free函数存在, 释放节点value的内存
    if (list->free) list->free(node->value);
    //释放节点内存
    zfree(node);
    //链表长度减 1
    list->len--;
}

/* Returns a list iterator 'iter'. After the initialization every
 * call to listNext() will return the next element of the list.
 *
 * This function can't fail. */
//获取链表迭代器指针
listIter *listGetIterator(list *list, int direction)
{
    //声明迭代器
    listIter *iter;

    //分配迭代器内存
    if ((iter = zmalloc(sizeof(*iter))) == NULL) return NULL;
    //如果是前面开始遍历
    if (direction == AL_START_HEAD)
        //遍历节点从head开始
        iter->next = list->head;
    else
        //从后面开始遍历
        iter->next = list->tail;
    //设置遍历方向
    iter->direction = direction;
    //返回指针
    return iter;
}

/* Release the iterator memory */
//释放迭代器对象
void listReleaseIterator(listIter *iter) {
    zfree(iter);
}

/* Create an iterator in the list private iterator structure */
//设置迭代器, 指定链表从head开始遍历
void listRewind(list *list, listIter *li) {
    li->next = list->head;
    li->direction = AL_START_HEAD;
}

//设置迭代器, 指定链表从 tail 开始遍历
void listRewindTail(list *list, listIter *li) {
    li->next = list->tail;
    li->direction = AL_START_TAIL;
}

/* Return the next element of an iterator.
 * It's valid to remove the currently returned element using
 * listDelNode(), but not to remove other elements.
 *
 * The function returns a pointer to the next element of the list,
 * or NULL if there are no more elements, so the classical usage
 * pattern is:
 *
 * iter = listGetIterator(list,<direction>);
 * while ((node = listNext(iter)) != NULL) {
 *     doSomethingWith(listNodeValue(node));
 * }
 *
 * */
//获取迭代器下一个节点
listNode *listNext(listIter *iter)
{
    //获取下一个节点
    listNode *current = iter->next;

    //下一个节点不为 NULL
    if (current != NULL) {
        //如果是从头开始遍历
        if (iter->direction == AL_START_HEAD)
            //遍历节点更新为下一个节点
            iter->next = current->next;
        else
            //从尾部开始遍历, 遍历节点更新为上一个节点
            iter->next = current->prev;
    }
    //返回当前节点
    return current;
}

/* Duplicate the whole list. On out of memory NULL is returned.
 * On success a copy of the original list is returned.
 *
 * The 'Dup' method set with listSetDupMethod() function is used
 * to copy the node value. Otherwise the same pointer value of
 * the original node is used as value of the copied node.
 *
 * The original list both on success or error is never modified. */
//复制链表
list *listDup(list *orig)
{
    //声明复制的链表
    list *copy;
    //声明迭代器
    listIter iter;
    //
    listNode *node;

    //创建空链表, 为 NULL, 则直接返回 NULL
    if ((copy = listCreate()) == NULL)
        return NULL;
    //继承原链表的相关函数
    copy->dup = orig->dup;
    copy->free = orig->free;
    copy->match = orig->match;
    //设置迭代器从头开始遍历
    listRewind(orig, &iter);
    //获取下一个节点
    while((node = listNext(&iter)) != NULL) {
        //值指针
        void *value;

        //如果有复制value的函数
        if (copy->dup) {
            //用函数拷贝值
            value = copy->dup(node->value);
            //如果为空, 则释放要拷贝的链表并且返回 NULL
            if (value == NULL) {
                listRelease(copy);
                return NULL;
            }
        } else
            //没有拷贝函数, 则直接获取值
            value = node->value;
        //添加值添加到copy链表的尾部, 内存分配失败则释放copy链表的内存并返回NULL
        if (listAddNodeTail(copy, value) == NULL) {
            listRelease(copy);
            return NULL;
        }
    }
    //返回拷贝的链表
    return copy;
}

/* Search the list for a node matching a given key.
 * The match is performed using the 'match' method
 * set with listSetMatchMethod(). If no 'match' method
 * is set, the 'value' pointer of every node is directly
 * compared with the 'key' pointer.
 *
 * On success the first matching node pointer is returned
 * (search starts from head). If no matching node exists
 * NULL is returned. */
//根据key查节点
listNode *listSearchKey(list *list, void *key)
{
    //声明迭代器
    listIter iter;
    //当前节点
    listNode *node;

    //设置迭代器
    listRewind(list, &iter);
    //遍历迭代器, 获取当前节点
    while((node = listNext(&iter)) != NULL) {
        //如果有比较函数
        if (list->match) {
            //比较当前节点的值, 相等则返回
            if (list->match(node->value, key)) {
                return node;
            }
        } else {
            //没有比较函数, 直接用 == 比较, 只能比较数值
            if (key == node->value) {
                return node;
            }
        }
    }
    //没找到, 则返回 NULL
    return NULL;
}

/* Return the element at the specified zero-based index
 * where 0 is the head, 1 is the element next to head
 * and so on. Negative integers are used in order to count
 * from the tail, -1 is the last element, -2 the penultimate
 * and so on. If the index is out of range NULL is returned. */
//获取指定下标的节点
listNode *listIndex(list *list, long index) {
    listNode *n;

    //如果 index 小于 0 , 则从后面遍历
    if (index < 0) {
        //取正数
        index = (-index)-1;
        //获取初始节点
        n = list->tail;
        //逐个索引迭代
        while(index-- && n) n = n->prev;
    } else {
        //从前面遍历
        n = list->head;
        //逐个索引迭代
        while(index-- && n) n = n->next;
    }
    //返回节点
    return n;
}

/* Rotate the list removing the tail node and inserting it to the head. */
//将链表尾节点变头节点
void listRotateTailToHead(list *list) {
    //长度小于等于一个节点, 不处理
    if (listLength(list) <= 1) return;

    /* Detach current tail */
    //获取链表尾节点
    listNode *tail = list->tail;
    //重置尾节点为上一个节点
    list->tail = tail->prev;
    //重置新的尾节点的下一个节点为NULL
    list->tail->next = NULL;
    /* Move it as head */
    //设置head的前一个节点为 tail
    list->head->prev = tail;
    //设置 tail 前一个节点为 NULL
    tail->prev = NULL;
    //更新 tail 的下一个节点为链表的 head
    tail->next = list->head;
    //重置 head 节点为 tail
    list->head = tail;
}

/* Rotate the list removing the head node and inserting it to the tail. */
//将头节点变尾节点, 代码基本同 listRotateTailToHead 一致
void listRotateHeadToTail(list *list) {
    if (listLength(list) <= 1) return;

    listNode *head = list->head;
    /* Detach current head */
    list->head = head->next;
    list->head->prev = NULL;
    /* Move it as tail */
    list->tail->next = head;
    head->next = NULL;
    head->prev = list->tail;
    list->tail = head;
}

/* Add all the elements of the list 'o' at the end of the
 * list 'l'. The list 'other' remains empty but otherwise valid. */
//将链表o拼接到链表l后面
void listJoin(list *l, list *o) {
    //如果要合并的链表长度为 0, 则不处理
    if (o->len == 0) return;

    //设置 o 的头节点的前节点为 l 的尾节点
    o->head->prev = l->tail;

    //如果l的尾节点不为NULL
    if (l->tail)
        //设置 l 的尾节点的下一个节点为 o 的头节点
        l->tail->next = o->head;
    else
        //如果l的尾节点为NULL, 则直接用o的首节点作用l的首节点
        l->head = o->head;

    //更新 l 的尾节点
    l->tail = o->tail;
    //更新l链表的长度
    l->len += o->len;

    /* Setup other as an empty list. */
    //清空o链表
    o->head = o->tail = NULL;
    o->len = 0;
}
