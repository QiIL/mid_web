## 伪目标, 跳过文件检测
.PHONY: all

all:
ifndef PUBLISH_MSG
	@(echo "$(shell \date +%Y%m%d%H%M%S)" > "publish_msg")
else
	@(echo "${PUBLISH_MSG}" > "publish_msg")
endif

## 生产环境和debug编译尽量相同
ifdef PROFILE
#	@(rebar3 as $(PROFILE) compile)
	@(rebar3 as $(PROFILE) release)
else
#	@(rebar3 compile) 
	@(rebar3 release)
endif