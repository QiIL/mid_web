## 一个web框架

### To do list

- [X] 使用cowboy，对http请求做通用处理
- [X] .config文件编译成.erl文件，对配置有通用的接口读取
- [X] lager日志接入，自定义
- [X] 时间库
- [X] 热更新
- [ ] sync
- [ ] 时间服务器（供长连接的进程使用）
- [ ] 发布打包脚本
- [ ] 数据库接入

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