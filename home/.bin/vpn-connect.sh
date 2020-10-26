#!/usr/bin/env expect

spawn totp TOKEN google
expect {
    \n {
        set result $expect_out(buffer)
        # puts $expect_out(buffer)
        exp_continue
    }
    eof {
        # puts $expect_out(buffer)
    }
}
send_user "command exit with totp: $result"


set timeout -1
spawn /opt/cisco/anyconnect/bin/vpn connect URL
match_max 100000

expect {
    "Username: " { send "acates\r" }
    "Username: \[acates\]" { send "\r" }
}

expect "Password: " { send "PASSWORD\r" }
expect "Answer: " { send "3\r" }
expect "Answer: " { send "${result}\r" }

set timeout 60
expect "VPN>"
