#!/usr/bin/env bash

echo "Application ID: $(apkanalyzer --human-readable manifest application-id "$@")"
echo "Version name: $(apkanalyzer --human-readable manifest version-name "$@")"
echo "Version code: $(apkanalyzer --human-readable manifest version-code "$@")"
# echo "Application ID, version code, and version name: $(apkanalyzer --human-readable apk summary "$@")"
echo "Total file size of the APK: $(apkanalyzer --human-readable apk file-size "$@")"
echo "Estimate of the download size of the APK: $(apkanalyzer --human-readable apk download-size "$@")"
echo "Features used by the APK: $(apkanalyzer --human-readable apk features "$@")"
echo "Minimum SDK version: $(apkanalyzer --human-readable manifest min-sdk "$@")"
echo "Target SDK version: $(apkanalyzer --human-readable manifest target-sdk "$@")"
printf "List of permission:\n%s\n" "$(apkanalyzer --human-readable manifest permissions "$@")"
echo "Is the app is debuggable: $(apkanalyzer --human-readable manifest debuggable "$@")"
