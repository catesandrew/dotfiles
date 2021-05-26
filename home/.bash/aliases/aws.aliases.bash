# load aws, if you are using it
if cask_contains_element "awscli"; then
  # export AWS_TARGET="$(brew --prefix awscli)"

  function __aws_cs3() {
    local bucket file tmpfile
    bucket=$(aws s3 ls | tr -s ' ' | cut -d' ' -f 3- | fzf)
    [ -n "$bucket" ] && file=$(aws s3 ls "$bucket" --recursive | tr -s ' ' | cut -d' ' -f 4- | fzf)
    if [ -n "$file" ]; then
      tmpfile=$(mktemp)
      aws s3 cp "s3://$bucket/$file" "$tmpfile" && cat "$tmpfile"
      rm "$tmpfile"
    fi
  }

  alias awscs3="__aws_cs3"
fi
