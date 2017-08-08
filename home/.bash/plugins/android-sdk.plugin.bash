# load android-sdk, if you are using it

if cask_contains_element "android-sdk" || \
    hash android 2>/dev/null; then
  export ANDROID_SDK_ROOT=/usr/local/share/android-sdk
fi
