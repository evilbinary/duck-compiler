#!/bin/bash
export CHEZSCHEMELIBDIRS=.:..

trap 'onCtrlC' INT
function onCtrlC () {
    echo 'Ctrl+C is captured'
    ps -ef |grep test. |grep -v grep| awk '{print $2'}|xargs kill -9 
}

cd build && scheme --script  ../$1 #--debug-on-exception
#cd tests  && echo '(parameterize ([run-cp0 (lambda (cp0 x) x)]) (load-program "../'$1'"))' | scheme -q