## 一个web框架

### To do list

- [X] 使用cowboy，对http请求做通用处理
- [X] .config文件编译成.erl文件，对配置有通用的接口读取
- [X] lager日志接入，自定义
- [X] 时间库
- [X] 热更新
- [X] Sync开发热更新
- [X] 发布打包脚本
- [X] 数据库接入
- [ ] 时间服务器（供长连接的进程使用）
- [ ] 数据使用`record`定义，读取写入都处理成`record`形式

### Getting Start
```bash
git clone https://github.com/QiIL/mid_web.git
cd mid_web
make all
cd /data/mid_web
./_build/default/rel/mid_web/ctrl start
curl 127.0.0.1:8080/api/comm/test
{"code":200,"data":"\"I am common controller\"","ret":1}
```

### Config
目录`./apps/config/src`

配置格式：
```erlang
-module(Mod).
-export([find/1]).
?CFG_H
?C(Key, Val)
?CFG_E.
```

### 路由
配置：

目录1`./apps/config/src/auth_router.erl

目录2`./apps/config/src/common_router.erl

其他自己加
```erlang
?C({<<Method>>, <<Action>>}, #r_router{controller=ControllerMod, func=Func})
```

### 日志

路径：`/data/log/mid_web`

配置：`sys.config.src -> lager`

### 时间库
`wtime app`使用原则，所有的时间戳都无时区，所有的`datetime`都有时区。

### 热更新

编译，源码为`script/compile_file.es`，默认编译路径为`_build/default/lib/App/ebin`
``` bash
cd mid_web
# 编译需要的模块，
./ctrl c 模块1 模块2 模块3...
# 只需要填模块名不需要.erl，例如：
./ctrl c wtime wlog
```
加载，源码为`script/load_module.es`，`ctrl`也有一些处理，通知运行中的节点加载指定的模块（不处理模块复制）。
``` bash
cd mid_web
./ctrl l 模块1 模块2 模块3...
```
开发场景一起使用
``` bash
cd mid_web
./ctrl cl 模块1 模块2 模块3...
```

### Sync开发热更新
不需要自己编译更新文件，SYNC使用项目目录+编译参数来进行热更新
``` erlang sys.config.src
{sync, [
        {log, all}, % 开发日志尽量多吧
        {src_dirs, 
            {add, [{"", []}]} % 没有定制的输出文件，项目内全部dir，根据默认的erl_opt outdir 编译到默认目录
        }
    ]}.
```

### 打包发布脚本
源码位置为：`./script/deploy.sh`和`./script/sed_config.es`shell写文件有点烦，偷懒用`escript`写
``` bash
./ctrl deploy
请设置节点ID(唯一正整数):  2
请设置web服务端口(默认为8080):
请输入发布目录(默认/data/deploy):
web服务的ip配置(默认为127.0.0.1):
请输入发布模式：1直接发布；2打tar包(默认为1):  1
/root/mid_web/_build/prod/rel/mid_web
部署参数如下:
  项目：mid_web
  节点ID: 2
  端口: 8080
  发布目录: /data/deploy
  IP: 127.0.0.1
确认用上述参数发布吗?(y/n/Enter)
复制文件成功！
初始化配置文件 env.config
env.config初始化成功！
发布完成，项目已发布到：/data/deploy/mid_web_2

```

### 数据库相关
数据库使用`mysql`，数据库读取封装了一下`mysql_poolboy`
1. 创建一个mid_web的database
2. 创建一个test的表
3. 我的使用例子
4. 字符串拼接和二进制拼接，貌似是字符串快一点:`wdb:test_speed()`
``` erlang
+----+------+------+
| id | col1 | col2 |
+----+------+------+

wdb:insert(test, [1,1,1]).
query: "INSERT INTO test VALUES ( ?, ?, ? ) "
 params: ["1","1","1"]
ok

wdb:read(test, 1).
SELECT * FROM test WHERE id = 1
{ok,[<<"id">>,<<"col1">>,<<"col2">>],[[1,1,1]]}

wdb:update(test1, 1, [{col1, 1000}]).
query: " UPDATE test1 SET col1 = 1000 WHERE id = 1"
 params: " UPDATE test1 SET col1 = 1000 WHERE id = 1"
ok

wdb:read(test, 1).
SELECT * FROM test WHERE id = 1
{ok,[<<"id">>,<<"col1">>,<<"col2">>],[[1,1000,1]]}

+----+------+------+
| id | col1 | col2 |
+----+------+------+
|  1 | 1000 |    1 |
+----+------+------+

wdb:insert_col(test1, [{col1, 100222}, {col2, 122002}]).
query: "INSERT INTO test2 ( col1, col ) VALUES ( ?, ? ) "
 params: ["100222","122002"]
ok

wdb:read(test1, 2).
SELECT * FROM test1 WHERE id = 2
{ok,[<<"id">>,<<"col1">>,<<"col2">>],
    [[3,<<"100222">>,<<"122002">>]]}

+----+--------+--------+
| id |  col1  |  col2  |
+----+--------+--------+
|  1 |   1000 |     1  |
+----+--------+--------+
|  2 | 100222 | 122002 |
+----+--------+--------+

```



