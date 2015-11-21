--
-- OCR all documents in a folder and all sub-folders
--
set theFolder to (choose folder with prompt "Choose Folder to OCR every PDF in recursively descending")
ocr_this_folder(theFolder)

on ocr_pdf(PDFfilename)
	tell application "PDFpenPro 6"
		open PDFfilename
		set theDoc to document 1
		if needs ocr of theDoc then
			ocr theDoc
			repeat while performing ocr of theDoc
			end repeat
			save theDoc
		end if
		close theDoc
	end tell
end ocr_pdf

on ocr_this_folder(FolderName)
	tell application "Finder"
		set PDFFiles to (files of folder FolderName whose name extension is "pdf") as alias list
		-- set subFolders to every folder of entire contents of FolderName
		set subFolders to (folders of folder FolderName)
	end tell
	try
		repeat with i from 1 to number of items in PDFFiles
			set this_item to item i of PDFFiles
			ocr_pdf(this_item)
		end repeat
	on error errText
		display dialog "OCRMe Error: " & errText
	end try
	repeat with i from 1 to number of items in subFolders
		ocr_this_folder((item i of subFolders) as string)
	end repeat
end ocr_this_folder

