#! /bin/bash
######################################
# 打包发布脚本，用于提取一个生产环境编译过的包，
# 或者直接在本机部署一个节点
# 步骤
# 1. 检测./_build/prod/是否存在
# 2. 检查传入参数: id, port，输出目录等 
# 3. 复制./_build/prod/rel/mid_web到目标目录，或者输入的目录
# 4. 替换参数文件
# 5. 报告完成
#####################################
# source deploy.sh
DEPLOY_SH_DIR=$(cd $(dirname ${BASH_SOURCE[0]}); pwd)
ROOT_DIR=$(cd ${DEPLOY_SH_DIR}/../; pwd)
PROJECT_NAME=$1
shift
read -p "请设置节点ID(唯一正整数):  " SERVICE_NODE_ID
read -p "请设置web服务端口(默认为8080):  " SERVICE_PORT
read -p "请输入发布目录(默认/data/deploy):  " DEPLOY_DIR
read -p "web服务的ip配置(默认为127.0.0.1):  " SERVICE_IP
read -p "请输入发布模式：1直接发布；2打tar包(默认为1):  " DEPLOY_MOD

## ======================= func ============================
check_ip() {
    local IP=$1
    local VALID_CHECK=$(echo $IP|awk -F. '$1<=255&&$2<=255&&$3<=255&&$4<=255{print "yes"}')
    if echo $IP|grep -E "^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$">/dev/null; then
        if [ ${VALID_CHECK:-no} == "yes" ]; then
            #echo "IP $IP available."
            echo 0
        else
            #echo "IP $IP not available!"
            echo 1
        fi
    else
        #echo "IP format error!"
        echo 2
    fi
}

deploy_tar() {
  local NOW_TIME=$(date '+%Y%m%d_%H%M%S')
  /bin/cp -rf ${ROOT_DIR}/_build/prod/rel/${PROJECT_NAME} /tmp/${PROJECT_NAME}_${SERVICE_NODE_ID}_${NOW_TIME}
  echo "复制文件成功！"
  echo "初始化配置文件 env.config"
  local SED_CONFIG=$($ESCRIPT $DEPLOY_SH_DIR/sed_config.es /tmp/${PROJECT_NAME}_${SERVICE_NODE_ID}_${NOW_TIME}/config/env.config \
  node_id int $SERVICE_NODE_ID port int $SERVICE_PORT server_ip string $SERVICE_IP) 
  if [[ $SED_CONFIG -eq 0 ]]; 
  then 
    echo "env.config初始化成功！"
    echo "开始打包"
    tar -zcvf ${DEPLOY_DIR}/${PROJECT_NAME}_${SERVICE_NODE_ID}_${NOW_TIME}.tar.gz /tmp/${PROJECT_NAME}_${SERVICE_NODE_ID}_${NOW_TIME} 
    echo "打包完成，请查看：${DEPLOY_DIR}/${PROJECT_NAME}_${SERVICE_NODE_ID}_${NOW_TIME}.tar.gz"
  else
    exit 1
  fi
}

deploy_normal() {
  if [ -d $DEPLOY_DIR/${PROJECT_NAME}_${SERVICE_NODE_ID} ]
  then 
    echo "存在已发布的版本，请检查$DEPLOY_DIR"
    exit 1
  else
    mkdir -p /tmp
    local NOW_TIME=$(date '+%Y%m%d_%H%M%S')
    /bin/cp -rf ${ROOT_DIR}/_build/prod/rel/${PROJECT_NAME} /tmp/${PROJECT_NAME}_${NOW_TIME}
    echo "复制文件成功！"
    echo "初始化配置文件 env.config"
    local SED_CONFIG=$($ESCRIPT $DEPLOY_SH_DIR/sed_config.es /tmp/${PROJECT_NAME}_${NOW_TIME}/config/env.config \
    node_id int $SERVICE_NODE_ID port int $SERVICE_PORT server_ip string $SERVICE_IP) 
    if [[ $SED_CONFIG -eq 0 ]]; 
    then 
      echo "env.config初始化成功！"
      /bin/mv /tmp/${PROJECT_NAME}_${NOW_TIME} ${DEPLOY_DIR}/${PROJECT_NAME}_${SERVICE_NODE_ID}
      echo "发布完成，项目已发布到：${DEPLOY_DIR}/${PROJECT_NAME}_${SERVICE_NODE_ID}"
    else
      /bin/mv $DEPLOY_DIR/${PROJECT_NAME}_${SERVICE_NODE_ID} /tmp/${PROJECT_NAME}_${SERVICE_NODE_ID}
      exit 1
    fi
  fi
}
## ====================== func end ==========================

# 检查NODE_ID
if [ "$SERVICE_NODE_ID" == "" ] 
then 
  echo "节点ID不能为空"
  exit 1
else 
  if [[ $SERVICE_NODE_ID =~ ^[1-9][0-9]*$ ]];
  then
    :
  else
    echo "节点ID应该为正整数"
    exit 1
  fi
fi

# 检查端口号
if [ "$SERVICE_PORT" == "" ] 
then 
  SERVICE_PORT="8080"
else 
  if [[ $SERVICE_PORT =~ ^[1-9][0-9]*$ ]];
  then
    if [[ $SERVICE_PORT -gt 0 && $SERVICE_PORT -le 65535 ]];
    then :
    else
      echo "端口号的范围为0~65535"
      exit 1
    fi
  else
    echo "端口号应该为正整数"
    exit 1
  fi
fi

# 检查目录
if [ "$DEPLOY_DIR" == "" ]
then 
  mkdir -vp /data/deploy
  DEPLOY_DIR="/data/deploy"
else 
  if [ -d $DEPLOY_DIR ]
  then
    :
  else
    echo "发布目录不存在"
    exit 1
  fi
fi

# 检查IP格式
if [ "$SERVICE_IP" == "" ]
then 
  SERVICE_IP="127.0.0.1"
else
  CHECK_IP=$(check_ip $SERVICE_IP)
  if [[ $CHECK_IP -eq 0 ]];
  then 
    :
  elif [[ $CHECK_IP -eq 1 ]];
  then
    echo "请输入正确的IP地址"
    exit 1
  else
    echo "输入的IP格式错误"
    exit 1
  fi
fi

# 检查发布模式
if [ "$DEPLOY_MOD" == "" ]
then
  DEPLOY_MOD=1
elif [ $DEPLOY_MOD -eq 1 ]
then
  :
elif [ $DEPLOY_MOD -eq 2 ]
then
  :
else 
  echo "不支持的发布模式"
fi

# 检查是否有prod编译结果
echo ${ROOT_DIR}/_build/prod/rel/${PROJECT_NAME}

if [ -d ${ROOT_DIR}/_build/prod/rel/${PROJECT_NAME} ]
then 
  :
else
  echo "未检测到生产环境编译的项目输出，请先编译项目执行：PROFILE=prod make all"
  exit 1
fi

# 检查sed_config.es是否存在
if [ -f ${DEPLOY_SH_DIR}/sed_config.es ]
then
  :
else
  echo "文件缺失：${DEPLOY_SH_DIR}/sed_config.es，请检查项目完整性"
  exit 1
fi

echo "部署参数如下:"
echo "  项目：$PROJECT_NAME"
echo "  节点ID: $SERVICE_NODE_ID"
echo "  端口: $SERVICE_PORT" 
echo "  发布目录: $DEPLOY_DIR"
echo "  IP: $SERVICE_IP"


read -p "确认用上述参数发布吗?(y/n/Enter)" CONFIRM

if [[ "$CONFIRM" == "" || "$CONFIRM" == "Y" || "$CONFIRM" == "y" || "$CONFIRM" == "yes" || "$CONFIRM" == "YES" ]];
then 
  :
else 
  echo "fine goodbye ~.~ "
  exit 1
fi

if [ $DEPLOY_MOD -eq 1 ]
then 
  deploy_normal
else
  deploy_tar
fi