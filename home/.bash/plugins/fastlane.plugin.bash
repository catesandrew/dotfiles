# load fastlane, if you are using it

if cask_contains_element "fastlane" || \
    hash fastlane 2>/dev/null; then

  if [[ -d "${HOME}/.fastlane/bin" ]]; then
    path_munge "${HOME}/.fastlane/bin" "after"
  fi
fi
