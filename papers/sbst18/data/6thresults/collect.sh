# creates single transcript file cointaining data from all transcript.csv files
>single_transcript.csv;header="";for csv in $( find . -name "transcript.csv" ); do [[ -z $header ]] && header="first row" && head -n 1 "$csv" >> single_transcript.csv; awk '{$1=$1};1' "$csv" | grep . | tail -n +2 >> single_transcript.csv; done
