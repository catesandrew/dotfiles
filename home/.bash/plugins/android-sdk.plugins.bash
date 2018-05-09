# load android-sdk, if you are using it

# Accept the Android SDK Licenses
# yes | sdkmanager --licenses

if cask_contains_element "android-sdk" || \
    hash android 2>/dev/null; then

  if [[ -d "/usr/local/share/android-sdk" ]]; then
    export ANDROID_SDK_ROOT=/usr/local/share/android-sdk

    export ANDROID_HOME=/usr/local/share/android-sdk

    if [[ -d "${ANDROID_HOME}/tools/bin" ]]; then
      path_munge "${ANDROID_HOME}/tools/bin" "after"
    fi

    if [[ -d "${ANDROID_HOME}/platform-tools" ]]; then
      path_munge "${ANDROID_HOME}/platform-tools" "after"
    fi
  fi
fi
