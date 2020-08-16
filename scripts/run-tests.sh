#!/bin/bash
export CHEZSCHEMELIBDIRS=.:..
trap 'onCtrlC' INT
function onCtrlC () {
    echo 'Ctrl+C is captured'
    ps -ef |grep test. | grep -v grep|awk '{print $2'}|xargs kill -9 
}

cd build && scheme --script ../tests/test-all.ss
