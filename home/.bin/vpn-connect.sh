#!/usr/bin/env expect

# turn off the display to stdout.
log_user 0
spawn pass work/google-totp
expect {
    \n {
        # set token $expect_out(buffer,string)
        set token [string trim $expect_out(buffer)]
        exp_continue
    }
    eof {
        # puts $expect_out(buffer)
    }
}
send_user "command exit with token: $token"

spawn pass work/okta
expect {
    \n {
        set password [string trim $expect_out(buffer)]
        exp_continue
    }
    eof {
        # puts $expect_out(buffer)
    }
}
send_user "command exit with password: $password"

spawn totp $token google
expect {
    \n {
        # set result $expect_out(buffer)
        set result [string trim $expect_out(buffer)]
        exp_continue
    }
    eof {
        # puts $expect_out(buffer)
    }
}
send_user "command exit with totp: $result"

# turn display on again
log_user 1
set timeout -1
spawn /opt/cisco/anyconnect/bin/vpn connect https://vpn.happymoney.com
match_max 100000

expect {
    "Username: " { send "acates\r" }
    "Username: \[acates\]" { send "\r" }
}

expect "Password: " { send "${password}\r" }
expect "Answer: " { send "3\r" }
expect "Answer: " { send "${result}\r" }

set timeout 60
expect "VPN>"
