# 'AWS helper functions'

__aws_env() {
  PROFILE="${1:-${AWS_DEFAULT_PROFILE}}"
  echo "setup AWS $PROFILE"
  export AWS_ACCESS_KEY_ID=$(aws configure get aws_access_key_id --profile "$PROFILE");
  export AWS_SECRET_ACCESS_KEY=$(aws configure get aws_secret_access_key --profile "$PROFILE");
  export AWS_SESSION_TOKEN=$(aws configure get aws_session_token --profile "$PROFILE");
  export AWS_SECURITY_TOKEN=$(aws configure get aws_security_token --profile "$PROFILE");
  export AWS_DEFAULT_REGION=$(aws configure get region --profile "$PROFILE");
  echo "$PROFILE environment variables exported";
}

# about 'helper function for AWS credentials file'
function awskeys {
  if [[ $# -eq 1 ]] && [[ "$1" = "list" ]]; then
    __awskeys_list "$2"
  elif [[ $# -eq 1 ]] && [[ "$1" = "unset" ]]; then
    __awskeys_unset "$2"
  elif [[ $# -eq 2 ]] && [[ "$1" = "show" ]]; then
    __awskeys_show "$2"
  elif [[ $# -eq 2 ]] && [[ "$1" = "export" ]]; then
    __awskeys_export "$2"
  else
    __awskeys_help
  fi
}

function __awskeys_help {
  echo -e "Usage: awskeys [COMMAND] [profile]\n"
  echo -e "Helper to AWS credentials file.\n"
  echo -e "Commands:\n"
  echo "   help    Show this help message"
  echo "   list    List available AWS credentials profiles"
  echo "   show    Show the AWS keys associated to a credentials profile"
  echo "   export  Export an AWS credentials profile keys as environment variables"
  echo "   unset   Unset the AWS keys variables from the environment"
}

# When doing an `sts.assumerole` you get a third key called `AWS_SESSION_TOKEN`.
# Some tools refer to this as `AWS_SECURITY_TOKEN`.
function __awskeys_get {
  local ln=$(\grep -n "\[ *$1 *\]" ~/.aws/credentials | cut -d ":" -f 1)
  if [[ -n "${ln}" ]]; then
    tail -n +${ln} ~/.aws/credentials | \grep -E -m 3 "aws_access_key_id|aws_secret_access_key|aws_session_token"
  fi
}

function __awskeys_list {
  local credentials_list="$(\grep -E '^\[ *[a-zA-Z0-9_-]+ *\]$' ~/.aws/credentials)"
  if [[ -n $"{credentials_list}" ]]; then
    echo -e "Available credentials profiles:\n"
    for cred in ${credentials_list}; do
      echo "    $(echo ${cred} | tr -d "[]")"
    done
    echo
  else
    echo "No profiles found in credentials file"
  fi
}

function __awskeys_show {
  PROFILE="${1:-${AWS_DEFAULT_PROFILE}}"
  local p_keys="$(__awskeys_get $PROFILE)"
  if [[ -n "${p_keys}" ]]; then
    echo "${p_keys}"
  else
    echo "Profile '$PROFILE' not found in credentials file"
  fi
}

function __awskeys_export {
  PROFILE="${1:-${AWS_DEFAULT_PROFILE}}"
  local p_keys=( $(__awskeys_get $PROFILE | tr -d " ") )
  if [[ -n "${p_keys}" ]]; then
    for p_key in ${p_keys[@]}; do
      local key="${p_key%=*}"
      export "$(echo ${key} | tr [:lower:] [:upper:])=${p_key#*=}"
    done
    export AWS_DEFAULT_PROFILE="$PROFILE"
    export AWS_DEFAULT_REGION=$(aws configure get region --profile "$PROFILE");
  else
    echo "Profile '$PROFILE' not found in credentials file"
  fi
}

function __awskeys_unset {
  unset AWS_DEFAULT_PROFILE AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY AWS_SESSION_TOKEN
}

function __awskeys_comp {
  local cur prev opts prevprev
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD-1]}"

  opts="help list show export unset"

  case "${prev}" in
    help|list|unset)
      return 0
      ;;
    show|export)
      local profile_list="$(__awskeys_list | \grep "    ")"
      COMPREPLY=( $(compgen -W "${profile_list}" -- ${cur}) )
      return 0
      ;;
  esac

  COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )

  return 0
}

complete -F __awskeys_comp awskeys
