OPTIONS="\tShutdown\n\tReboot\n鈴\tSuspend\n\tLock\n"

option=`echo -e $OPTIONS | awk '{print $1}' | tr -d '\r\n\t'`
if [ "$@" ]
then
	case $@ in
		*Shutdown)
			shutdown now
			;;
		*Reboot)
			reboot
			;;
    *Suspend)
     systemctl suspend
     ;;
    *Lock)
      slock
      ;;
	esac
else
	echo -e $OPTIONS
fi

