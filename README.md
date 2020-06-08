# TRUMP-通用TCP&UDP服务协议
## 1. 简介

​        看到TRUMP大家首先想到的是一头飘逸金发，胸前飘着鲜艳红领带的老年人，但是今天我说的这个TRUMP和上面这位半毛钱关系都没有。我的TRUMP有单独的含义，我给出的这个TTUMP是一种简单的客户端-服务端的通信协议。下面是TRUMP的完整名称：

- TRUMP：TCP Remix UDP Middle Protocol（中文释义：TCP混合UDP的中间层协议）

## 2. 技术

| 名称       | 版本  |
| ---------- | ----- |
| Erlang/OTP | 22.0+ |
| Mysql      | 8.0+  |
| Rebar3     | 3.0+  |



## 3. 编译

```shell
## 单元测试
rebar3 ct
## 编译
rebar3 compile
## 命令行交互式调试
rebar3 shell
## 发布
rebar3 release -n trump-1.0
```



## 5. 协议规范

​        TRUMP协议其实是2个模式组成的：TCP模式和UDP模式，TRUMP有一个固定数据报文头，数据固定报文头使用4个字节(byte)来表示，如下表所示：

| -------- | bit-0    | bit-1    | bit-2    | bit-3   | bit-4    | bit-5    | bit-6    | bit-7    |
| ----- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| 字节0 | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    |
| 字节1 | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    |
| 字节2 | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    |
| 字节3 | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    |

- 字节0表示消息报文类型，目前留64个值，实际用不到这么多；

- 字节1、2字节表示消息的长度（Byte），最长支持2^16=65536(Byte)个字符,也就是64KB，最长只能发送64KB数据；

- 字节3为扩展字节，预留给其他报文使用。分为高四位和低四位，主要用来传递客户端ID的长度。客户端ID长度最长为4位，也就是16个字符。当单个客户端的时候，ID在低四位，如果是客户端之间的通信，ID在高四位，而对端ID是低四位。

>  注意：因为服务端支持分布式，所以ID不可重复，必须是整个节点群内的唯一值，这里我采用了Erlang的UUID来实现。

## 4. 场景

​        TRUMP的核心是同时支持基于原生TCP（Raw TCP）和原生UDP（Raw UDP）两种类型的通信形式，对于这两种传输层协议做了最简单的封装，故命名为中间层协议（Middle Protocol）。
​        本协议的设计相对来讲比较通俗易懂，都是基础知识，可以为初学者提供一些设计思路，这些思路可以用在游戏开发，IM开发，还有物联网终端通信等等，其实就是提供了一个把传输层往应用层靠拢的最基础的思路。

## 5.客户端数据表
```sql
CREATE TABLE `client` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(200) DEFAULT NULL COMMENT 'MQTT用户名',
  `client_id` varchar(200) DEFAULT NULL COMMENT 'MQTT ClientID',
  `password` varchar(200) DEFAULT NULL COMMENT 'MQTT密码',
  `token` varchar(255) DEFAULT NULL COMMENT '认证token',
  `description` varchar(200) DEFAULT NULL COMMENT '描述',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8mb4 ROW_FORMAT=DYNAMIC;
```

## 6.说明

​该设计当然是不完善的，远远达不到工业级的水准，所以各位朋友如果发现问题或者设计思路有大问题，可以指出。但是不接受喷子和高论，如果某个大佬有高论或者更优秀的设想，请自己亲自实现一遍比较好。同时欢迎有兴趣的人一起讨论物联网技术话题。
讲文明树新风，文明社会树立文明形象。祝大家技术越来越牛逼。
