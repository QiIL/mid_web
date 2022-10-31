## 一个web框架

### To do list

- [X] 使用cowboy，对http请求做通用处理
- [X] .config文件编译成.erl文件，对配置有通用的接口读取
- [X] lager日志接入，自定义
- [X] 时间库
- [ ] 热更新
- [ ] 时间服务器（供长连接的进程使用）
- [ ] 数据库接入

### Getting Start
```bash
git clone https://github.com/QiIL/mid_web.git
cd mid_web
make all
cd /data/mid_web
./ctrl start
curl 192.168.100.42:8080/api/comm/test
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


