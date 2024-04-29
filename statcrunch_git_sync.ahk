#SingleInstance Force
#Persistent

FileGetSize, fileSize, LaTeX_report/final_report.tex
gosub, SyncReport

SetTimer, SyncReport, 300000
return

SyncReport:
FileGetSize, fileSizeLatest, LaTeX_report/final_report.tex
if(fileSizeLatest != FileSize) {
	fileSize := fileSizeLatest
	gosub, SendCommands
} else {
	Loop {
	FileGetSize, fileSizeLatest, LaTeX_report/final_report.tex
		if(fileSizeLatest != fileSize) {
			fileSize := fileSizeLatest
			gosub, SendCommands
			break
		}
	Sleep, 60000
	}
}
return

SendCommands:
RunWait, git add LaTeX_report/final_report.tex Twitch_Streamer_Data_2023.scs,, Hide
RunWait, git commit -m "Auto commit",, Hide
RunWait, git push,, Hide
return