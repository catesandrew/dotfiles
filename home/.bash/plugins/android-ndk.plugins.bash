# load android-ndk, if you are using it

# Accept the Android SDK Licenses
# yes | sdkmanager --licenses

if cask_contains_element "android-ndk"; then
  if [[ -d "${BREW_HOME}/share/android-ndk" ]]; then
    export ANDROID_NDK_HOME=${BREW_HOME}/share/android-ndk
    function emulator { ( cd "$(dirname "$(readlink -f "$(which emulator)")"  )" && ./emulator "$@"; ) }
  fi
fi
