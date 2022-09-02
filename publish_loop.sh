for (( ; ; ))
do
  echo "infinite loop [ hit CTRL+C to stop]"
	      

  echo "sleeping until tomorrow 8am" 
  sleep $(( $(date -f - +%s- <<< "tomorrow 08:00"$'\nnow') 0 ))
  ./do_all_new.sh && ./copy_and_push.sh
		       
done


