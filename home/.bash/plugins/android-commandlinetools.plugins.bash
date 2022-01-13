# load android-sdk, if you are using it

# Accept the Android SDK Licenses
# yes | sdkmanager --licenses

if cask_contains_element "android-commandlinetools"; then
  if [[ -d "/usr/local/share/android-commandlinetools" ]]; then
    export ANDROID_SDK_ROOT=/usr/local/share/android-commandlinetools

    export ANDROID_HOME=/usr/local/share/android-commandlinetools

    if [[ -d "${ANDROID_HOME}/emulator" ]]; then
      path_munge "${ANDROID_HOME}/emulator" "after"
    fi
  fi
fi
