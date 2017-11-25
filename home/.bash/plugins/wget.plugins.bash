if wget --help | grep -q "local-encoding"; then
    true
    # modify ~/.wgetrc
    # local_encoding = UTF-8
    # remote_encoding = UTF-8
fi
