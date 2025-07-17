# clap - a BASH argument parser
# This parser aims to have similar parsing semantics as Rust's clap parser; if it doesn't look anything like clap it's not just you.

clap_usage=""
clap_flag_match=""
clap_defaults=""
clap_arguments_string=""

# -----------------------------------------------------------------------------------------------------------------------------
function clap.throw_error() {
	local message="$1"
	echo "CLAP: ERROR: $message"
	exit 1
}

# -----------------------------------------------------------------------------------------------------------------------------
function clap.define() {
	if [ $# -lt 3 ]; then
		clap.throw_error "clap.define <short> <long> <variable> [<desc>] [<default>] [<value>] [<nargs>]"
	fi
	local nargs=""
	for option_id in $(seq 1 $#); do
		local option
		option="$(eval "echo \$$option_id")"
		local key
		key="$(echo "$option" | awk -F "=" '{print $1}')"
		local value
		value="$(echo "$option" | awk -F "=" '{print $2}')"

		#essentials: shortname, longname, description
		if [ "$key" = "short" ]; then
			local shortname="$value"
			if [ ${#shortname} -ne 1 ]; then
				clap.throw_error "short name expected to be one character long"
			fi
			local short="-${shortname}"
		elif [ "$key" = "long" ]; then
			local longname="$value"
			if [ ${#longname} -lt 2 ]; then
				clap.throw_error "long name expected to be at least one character long"
			fi
			local long="--${longname}"
		elif [ "$key" = "desc" ]; then
			local desc="$value"
		elif [ "$key" = "default" ]; then
			local default="$value"
		elif [ "$key" = "variable" ]; then
			local variable="$value"
		elif [ "$key" = "value" ]; then
			local val="$value"
		elif [ "$key" = "nargs" ]; then
			local nargs="$value"
		fi
	done

	if [ "$variable" = "" ]; then
		clap.throw_error "You must give a variable for option: (${short:-}/${long:-})"
	fi

	if [ "${val:-}" = "" ]; then
		val="\$OPTARG"
	fi

	# build OPTIONS and help
	clap_usage="${clap_usage}#NL#TB${short:-} $(printf "%-25s %s" "${long}:" "${desc}")"
	if [ "${default:-}" != "" ] && [ "${nargs:-}" != "0" ]; then
		clap_usage="${clap_usage} [default:$default]"
	fi
	clap_flags="${clap_flags:-} ${long}"
	if [ "${nargs:-}" == "" ]; then
		clap_flag_match="${clap_flag_match}#NL#TB#TB${long}${short:+|${short}})#NL#TB#TB#TB${variable}=\"\$1\"; shift 1;;"
	elif [ "${nargs:-}" == "0" ]; then
		clap_flag_match="${clap_flag_match}#NL#TB#TB${long}${short:+|${short}})#NL#TB#TB#TB${variable}=\"true\";;"
	else
		clap_flag_match="${clap_flag_match}#NL#TB#TB${long}${short:+|${short}})#NL#TB#TB#TB${variable}=(); for ((i=0; i<nargs; i++)); do ${variable}+=( \"\$1\" ); shift 1; done;;"
	fi
	if [ "${default:-}" != "" ]; then
		clap_defaults="${clap_defaults}#NL${variable}=${default@Q}"
	fi
	clap_arguments_string="${clap_arguments_string}${shortname:-}"
	if [ "${val:-}" = "\$OPTARG" ]; then
		clap_arguments_string="${clap_arguments_string}:"
	fi
}

# -----------------------------------------------------------------------------------------------------------------------------
function clap.build() {
	local build_file
	build_file="$(mktemp -t "clap-XXXXXX.tmp")"

	# Function usage
	cat <<EOF >"$build_file"
function usage(){
cat << XXX
usage: \$(basename "\$0") [OPTIONS]

OPTIONS:
        ${clap_usage:-}

        -? --help  :  usage

        --verbose  :  show debug info

XXX
}

# Autocomplete
# Manual: https://www.gnu.org/software/bash/manual/html_node/Programmable-Completion.html
[[ "\${COMP_LINE:-}" == "" ]] || [[ "\${COMP_POINT:-}" == "" ]] || {
	COMP_CURRENT="${1:-}"
	case "\$COMP_CURRENT" in
	"")	compgen -W "${clap_flags:-}" -- "\$COMP_CURRENT" ;;
	-*)	compgen -W "${clap_flags:-}" -- "\$COMP_CURRENT" ;;
	*)	compgen -f -- "\$COMP_CURRENT" ;;
	esac
	exit 0
}

# Set default variable values
$clap_defaults

# Contract long options into short options
while [ \$# -ne 0 ]; do
        param="\$1"
        shift 1

        case "\$param" in
                $clap_flag_match
                "-?"|--help)
			[[ "$(type -t print_help)" != function ]] || print_help
			echo
                        usage
                        exit 0;;
		--verbose)
			CLAP_VERBOSE=true
			set -x;;
		--)
			break ;;
                -*)
                        echo -e "Unrecognized option: \$param"
                        usage
                        exit 1 ;;
                *)
			set "\$param" "\${@}"
			break ;;
        esac
done

# Clean up after self
rm $build_file

EOF

	# shellcheck disable=SC2094
		cat <<<"$(sed 's/#NL/\n/g' "$build_file")" > "$build_file"
	# shellcheck disable=SC2094
		cat <<<"$(sed "s/#TB/\t/g" "$build_file")" > "$build_file"

	# Unset global variables
	unset clap_usage
	unset clap_arguments_string
	unset clap_defaults
	unset clap_flag_match

	# Return file name to parent
	echo "$build_file"
}
# -----------------------------------------------------------------------------------------------------------------------------
