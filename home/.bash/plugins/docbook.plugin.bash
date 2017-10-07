if brew_contains_element "docbook" || \
    hash docbook 2>/dev/null; then
  export XML_CATALOG_FILES="/usr/local/etc/xml/catalog"
fi
