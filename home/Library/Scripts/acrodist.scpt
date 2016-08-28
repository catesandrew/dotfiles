-- osascript -e distiller.applescript cdpmc81daWUwZri.ps "/Library/Application Support/Adobe/Adobe PDF/Settings/High Quality Print.joboptions"

on run argv
  -- set unixPath to POSIX path of ((path to me as text) & "::")
  -- set LaunchCrossFire to "/usr/local/bin/wish '" & UnixPath & "CrossFire.tcl' > /dev/null 2>&1 &"
  -- do shell script LaunchCrossFire

  tell application "Finder" to get folder of (path to me) as Unicode text
  set workingPath to POSIX path of result
  set inFile to workingPath & item 1 of argv
  -- set inFile to item 1 of argv
  -- set inFilePath to POSIX path of inFile
  -- set inFilePath to POSIX path of (inFile & ":")
  -- set parentDirectory to POSIX file (do shell script "dirname " & quoted form of inFile) as alias
  set parentDirectory to POSIX path of (do shell script "dirname " & quoted form of inFile)
  -- set myPDFSettingsPath to item 2 of argv

  with timeout of (8 * 60) seconds
    tell application "Acrobat Distiller"
      -- Distill sourcePath item 1 of argv
      -- Distill sourcePath inFile destinationPath parentDirectory adobePDFSettingsPath myPDFSettingsPath
      Distill sourcePath inFile destinationPath parentDirectory
      run
      quit
    end tell
  end timeout
end run
