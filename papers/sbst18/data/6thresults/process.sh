# create error files for all subfolders
for i in * ; do printf $i; find $i -type f -name *.log -exec grep -nH -e "ERROR" {} + > $i/errors.txt ; done

