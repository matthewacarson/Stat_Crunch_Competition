#SingleInstance Force
#Persistent

FileGetSize, Size, LaTeX_report/final_report.tex
fileSize := Size
FileGetSize, Size, Twitch_Streamer_Data_2023.scs
fileSize += Size
FileGetSize, fileSize, Twitch_Streamer_Data_2023.csv
fileSize += Size

timeInterval := 5 * 60 * 1000

SetTimer, SyncReport, %timeInterval%
return

SyncReport:
gosub, GetLatestFileSize
if(fileSizeLatest != fileSize) {
	fileSize := fileSizeLatest
	gosub, SendCommands
} else {
	Loop {
		Sleep, 60000
		gosub, GetLatestFileSize
		if(fileSizeLatest != fileSize) {
			fileSize := fileSizeLatest
			gosub, SendCommands
			break
		}
	}
}
return

GetLatestFileSize:
FileGetSize, SizeLatest, LaTeX_report/final_report.tex
fileSizeLatest := SizeLatest
FileGetSize, SizeLatest, Twitch_Streamer_Data_2023.scs
fileSizeLatest += SizeLatest
FileGetSize, SizeLatest, Twitch_Streamer_Data_2023.csv
fileSizeLatest += SizeLatest
return


SendCommands:
RunWait, git add LaTeX_report/final_report.tex Twitch_Streamer_Data_2023.scs Twitch_Streamer_Data_2023.csv,, Hide
RunWait, git commit -m "Auto commit",, Hide
;~ RunWait, git push,, Hide
return