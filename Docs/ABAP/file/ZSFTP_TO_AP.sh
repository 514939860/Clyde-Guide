#! /bin/bash

###
# @Author: Ver Ver_Guo@wistron.com
# @Date: 2023-09-05 11:24:51
# @LastEditors: Clyde Clyde_Liu@wistron.com
# @LastEditTime: 2024-05-14 11:16:35 Overwrite by github copilot
# @Description: Download file from SFTP server to AP server
###

# -h host
# -p port
# -u username
# -w password
# -o original sftp file path            -b /root/aaa.txt
# -of
# -t target AP server file path         -b /root/aaa.txt
# -tf
# -b back up file path in original server -b /root/aaa.txt  --TBD
# -z compress file or not   -z X                            --TBD
# -ctp  connect type ssh-dss
# Usage: ZBAMA024_SFTP_O_2.sh -h 10.30.118.xxx -u wksvxx -w  xxxx -o /interface/temp/ -t /interface/sftp/sap/ -of F601.txt -tf vgtest.txt -ctp ssh-dss

declare -A parameters=()

checkParameter() {
    local parameter=$1
    local message=$2
    if [ "${parameters[$parameter]+x}" != "x" ]; then
        echo "$message"
        exit 1
  fi
}

getParameters() {
    local idx=0
    local p=""
    for i in "$@"; do
        ((idx++))
        if ((idx == 1)); then
            continue
    fi

        if ((idx % 2 != 0)); then
            case "$p" in
            "-h") parameters["host"]=$i ;;
            "-p") parameters["port"]=$i ;;
            "-u") parameters["username"]=$i ;;
            "-w") parameters["password"]=$i ;;
            "-o") parameters["ori_fpath"]=$i ;;
            "-of") parameters["ori_fname"]=$i ;;
            "-t") parameters["target_fpath"]=$i ;;
            "-tf") parameters["target_fname"]=$i ;;
            "-b") parameters["back_up_ori_path"]=$i ;;
            "-z") parameters["compress"]=$i ;;
            # -ctp "ssh-dss"
            "-ctp") parameters["connect_type"]=$i ;;
            *)
                echo "Parameters format error: $p, Please reference format: -h 'xxx' -u 'xxx' -p 'xxx'"
                return 1
                ;;
      esac
    else
            p=$i
    fi
  done
    return 0
}

# error: parse: missing redirection filename
Download_files() {
    local connect_type=$1
    local connect_program=""
    connect_program="set sftp:connect-program 'ssh -F /dev/null -a -x -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null'"

    if [ "$connect_type" = "ssh-dss" ]; then
        connect_program="set sftp:connect-program 'ssh -F /dev/null -a -x -oHostKeyAlgorithms=+ssh-dss -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null'"
  fi

    lftp -d -u "${parameters["username"]}","${parameters["password"]}" sftp://"${parameters["host"]}:${parameters["port"]}" -e "
    set net:max-retries 3
    set net:reconnect-interval-multiplier 1
    set net:reconnect-interval-base 3
    set net:timeout 3
    set xfer:clobber on
    $connect_program
    lcd ${parameters["target_fpath"]} || exit 1
    cd ${parameters["ori_fpath"]} || exit 1
    mget ${parameters["ori_fname"]}
    bye
    "

    if [ $? -eq 0 ]; then
        echo "=======Download file from SFTP to AP server successfully====="
  else
        echo "=======Download failure======="
        exit 1
  fi
}

if ! getParameters parameters "$@"; then
    exit $?
fi

checkParameter "host" "Please provide target SFTP server host!"
checkParameter "username" "Please provide target SFTP server username!"
checkParameter "password" "Please provide target SFTP server password!"
checkParameter "ori_fpath" "Please provide the AP server's file path which you need send!"
checkParameter "ori_fname" "Please provide the AP server's file name which you need send!"
checkParameter "target_fpath" "Please provide target SFTP server path for file storage!"

if [ "${parameters["port"]+x}" != "x" ]; then
    parameters["port"]=22
fi

if [ "${parameters["target_fname"]+x}" != "x" ]; then
    parameters["target_fname"]=${parameters["ori_fname"]}
fi

# send with ${parameters["connect_type"]
Download_files "${parameters["connect_type"]}"
