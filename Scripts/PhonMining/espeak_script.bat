ECHO "Press any key to transcribe all languages"
PAUSE
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Catalan.txt -vca --ipa --phonout=%~dp0phon_transcriptions\Catalan_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Czech.txt -vcs --ipa --phonout=%~dp0phon_transcriptions\Czech_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Danish.txt -vda --ipa --phonout=%~dp0phon_transcriptions\Danish_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\English.txt -ven-us --ipa --phonout=%~dp0phon_transcriptions\English_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\French.txt -vfr --ipa --phonout=%~dp0phon_transcriptions\French_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\German.txt -vde --ipa --phonout=%~dp0phon_transcriptions\German_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Irish.txt -vga --ipa --phonout=%~dp0phon_transcriptions\Irish_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Italian.txt -vit --ipa --phonout=%~dp0phon_transcriptions\Italian_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Latvian.txt -vlv --ipa --phonout=%~dp0phon_transcriptions\Latvian_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Lithuanian.txt -vlt --ipa --phonout=%~dp0phon_transcriptions\Lithuanian_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f "%~dp0espeak_lists\Modern Greek.txt" -vel --ipa --phonout="%~dp0phon_transcriptions\Modern Greek_phon.txt" -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Polish.txt -vpl --ipa --phonout=%~dp0phon_transcriptions\Polish_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Portuguese.txt -vpt --ipa --phonout=%~dp0phon_transcriptions\Portuguese_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Romanian.txt -vro --ipa --phonout=%~dp0phon_transcriptions\Romanian_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Swedish.txt -vsv --ipa --phonout=%~dp0phon_transcriptions\Swedish_phon.txt -q
START "" "C:\Program Files\eSpeak NG\espeak-ng" -f %~dp0espeak_lists\Welsh.txt -vcy --ipa --phonout=%~dp0phon_transcriptions\Welsh_phon.txt -q