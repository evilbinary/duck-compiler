# 🦆鸭子编译器
鸭子编译器是一款基于scheme的魔改的鸭语言编译器
鸭子QQ群号:239401374

## 使用
linux依赖: 
```
sudo apt-get install  nasm
sudo dpkg --add-architecture i386
sudo apt-get install gcc-multilib libc6:i386 libncurses5:i386 libstdc++6:i386
```

osx 依赖: `brew install nasm`

运行: `make build`

## 鸭语言

自定义语法，声明变量，有为定义，值为“老王”
```c
《鸭语法》
有一个鸭，它叫“老王”。
打你鸭，走你鸭。
```

库定义，固定语法
```c
《树木》
-四言一三
移动树木，到三百米。
当三百米，锯成两段。
裁剪树枝，摘掉树叶。
```

函数定义
```c
《鸭子标准库》
-作者：鸭子
  定义：输出，名；
  打印：名。
```

函数调用
```c
《鸭子标准库》
输出：“嘎嘎”。
```

调用c
```c
《C》
输出：1234。
```


## 计划

1. add duck compiler [done]
2. add lib support vector and so on [doing]
3. add duck language support [doing]
4. add x86[done] x86-64 llvm[done] arm arm64 wasm
5. add duck os [doing]
6. add duck robot [doing]