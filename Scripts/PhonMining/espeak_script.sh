#!/bin/bash
read -n 1 -r -s -p $'Press any key to transcribe all languages...\n'
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "${parent_path}/espeak_lists"

echo "Azerbaijani"
espeak-ng -f Azerbaijani.txt -vaz --ipa --phonout=${parent_path}/phon_transcriptions/Azerbaijani_phon.txt
echo "Bengali"
espeak-ng -f Bengali.txt -vbn --ipa --phonout=${parent_path}/phon_transcriptions/Bengali_phon.txt -q
echo "Bulgarian"
espeak-ng -f Bulgarian.txt -vbg --ipa --phonout=${parent_path}/phon_transcriptions/Bulgarian_phon.txt -q
echo "Catalan"
espeak-ng -f Catalan.txt -vca --ipa --phonout=${parent_path}/phon_transcriptions/Catalan_phon.txt -q
echo "Czech"
espeak-ng -f Czech.txt -vcs --ipa --phonout=${parent_path}/phon_transcriptions/Czech_phon.txt -q
echo "Danish"
espeak-ng -f Danish.txt -vda --ipa --phonout=${parent_path}/phon_transcriptions/Danish_phon.txt -q
echo "English"
espeak-ng -f English.txt -ven-us --ipa --phonout=${parent_path}/phon_transcriptions/English_phon.txt -q
echo "Estonian"
espeak-ng -f Estonian.txt -vet --ipa --phonout=${parent_path}/phon_transcriptions/Estonian_phon.txt -q
echo "Finnish"
espeak-ng -f Finnish.txt -vfi --ipa --phonout=${parent_path}/phon_transcriptions/Finnish_phon.txt -q
echo "French"
espeak-ng -f French.txt -vfr --ipa --phonout=${parent_path}/phon_transcriptions/French_phon.txt -q
echo "German"
espeak-ng -f German.txt -vde --ipa --phonout=${parent_path}/phon_transcriptions/German_phon.txt -q
echo "Guarani"
espeak-ng -f Guaraní.txt -vgn --ipa --phonout=${parent_path}/phon_transcriptions/Guaraní_phon.txt -q
echo "Hawaiian"
espeak-ng -f Hawaiian.txt -vhaw --ipa --phonout=${parent_path}/phon_transcriptions/Hawaiian_phon.txt -q
echo "Hindi"
espeak-ng -f Hindi.txt -vhi --ipa --phonout=${parent_path}/phon_transcriptions/Hindi_phon.txt -q
echo "Hungarian"
espeak-ng -f Hungarian.txt -vhu --ipa --phonout=${parent_path}/phon_transcriptions/Hungarian_phon.txt -q
echo "Irish"
espeak-ng -f Irish.txt -vga --ipa --phonout=${parent_path}/phon_transcriptions/Irish_phon.txt -q
echo "Italian"
espeak-ng -f Italian.txt -vit --ipa --phonout=${parent_path}/phon_transcriptions/Italian_phon.txt -q
echo "Latvian"
espeak-ng -f Latvian.txt -vlv --ipa --phonout=${parent_path}/phon_transcriptions/Latvian_phon.txt -q
echo "Lithuanian"
espeak-ng -f Lithuanian.txt -vlt --ipa --phonout=${parent_path}/phon_transcriptions/Lithuanian_phon.txt -q
echo "Maori"
espeak-ng -f Maori.txt -vmi --ipa --phonout=${parent_path}/phon_transcriptions/Maori_phon.txt -q
echo "Modern Greek"
espeak-ng -f "Modern Greek.txt" -vel --ipa --phonout=${parent_path}"/phon_transcriptions/Modern Greek_phon.txt" -q
echo "Nepali"
espeak-ng -f Nepali.txt -vne --ipa --phonout=${parent_path}/phon_transcriptions/Nepali_phon.txt -q
echo "Nogai"
espeak-ng -f Nogai.txt -vnog --ipa --phonout=${parent_path}/phon_transcriptions/Nogai_phon.txt -q
echo "Persian"
espeak-ng -f Persian.txt -vfa --ipa --phonout=${parent_path}/phon_transcriptions/Persian_phon.txt -q
echo "Polish"
espeak-ng -f Polish.txt -vpl --ipa --phonout=${parent_path}/phon_transcriptions/Polish_phon.txt -q
echo "Portuguese"
espeak-ng -f Portuguese.txt -vpt --ipa --phonout=${parent_path}/phon_transcriptions/Portuguese_phon.txt -q
echo "Punjabi"
espeak-ng -f Punjabi.txt -vpa --ipa --phonout=${parent_path}/phon_transcriptions/Punjabi_phon.txt -q
echo "Romanian"
espeak-ng -f Romanian.txt -vro --ipa --phonout=${parent_path}/phon_transcriptions/Romanian_phon.txt -q
echo "Russian"
espeak-ng -f Russian.txt -vru --ipa --phonout=${parent_path}/phon_transcriptions/Russian_phon.txt -q
echo "Serbo-Croatian"
espeak-ng -f Serbo-Croatian.txt -vsr --ipa --phonout=${parent_path}/phon_transcriptions/Serbo-Croatian_phon.txt -q
echo "Spanish"
espeak-ng -f Spanish.txt -ves-419 --ipa --phonout=${parent_path}/phon_transcriptions/Spanish_phon.txt -q
echo "Swedish"
espeak-ng -f Swedish.txt -vsv --ipa --phonout=${parent_path}/phon_transcriptions/Swedish_phon.txt -q
echo "Tamil"
espeak-ng -f Tamil.txt -vta --ipa --phonout=${parent_path}/phon_transcriptions/Tamil_phon.txt -q
echo "Telugu"
espeak-ng -f Telugu.txt -vte --ipa --phonout=${parent_path}/phon_transcriptions/Telugu_phon.txt -q
echo "Vietnamese"
espeak-ng -f Vietnamese.txt -vvi --ipa --phonout=${parent_path}/phon_transcriptions/Vietnamese_phon.txt -q
echo "Welsh"
espeak-ng -f Welsh.txt -vcy --ipa --phonout=${parent_path}/phon_transcriptions/Welsh_phon.txt -q