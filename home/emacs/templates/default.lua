${1:$$(my/yas-choose-license)}

---------------------------
-- Source initialization --
---------------------------

source = {
  id              = "`(file-name-sans-extension (buffer-name))`",
  name            = "$2",
  description     = "$3",
  supported_keys  = { "$4" },
  supported_media = "${5:$$(yas-choose-value '("none" "audio" "video" "image" "all"))}",
  tags            = { "$6" }
}

------------------
-- Source utils --
------------------

$0

