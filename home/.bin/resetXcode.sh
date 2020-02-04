#!/bin/bash
killall Xcode
xcrun -k
xcodebuild -alltargets clean
rm -rf "$(getconf DARWIN_USER_CACHE_DIR)/org.llvm.clang/ModuleCache"
rm -rf "$(getconf DARWIN_USER_CACHE_DIR)/org.llvm.clang.$(whoami)/ModuleCache"
rm -rf ~/Library/Developer/Xcode/DerivedData/*
rm -rf ~/Library/Caches/com.apple.dt.Xcode/*

# open /Applications/Xcode.app
TARGET=(*.xcworkspace)
if [ $TARGET == "*.xcworkspace" ]; then
    TARGET=(*.xcodeproj)
    if [ $TARGET == "*.xcodeproj" ]; then
        TARGET=""
    fi
fi
if [ -z "$TARGET" ]; then
    open -a "/Applications/Xcode.app" "$TARGET"
else
    echo "Xcode workspace or project not found"
fi
