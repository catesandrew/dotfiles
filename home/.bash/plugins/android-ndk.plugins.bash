# load android-ndk, if you are using it

# Accept the Android SDK Licenses
# yes | sdkmanager --licenses

if cask_contains_element "android-ndk"; then
  if [[ -d "/usr/local/share/android-ndk" ]]; then
    export ANDROID_NDK_HOME=/usr/local/share/android-ndk
  fi
fi
