#!/usr/bin/env python
"""
    small script to check for unread count on imap inbox
"""
import sys
import imaplib

IMAPSERVER = sys.argv[1]
USER = sys.argv[2]
PASSWORD = sys.argv[3]

try:
    mail = imaplib.IMAP4_SSL(IMAPSERVER)
    mail.login(USER, PASSWORD)
    mail.select("inbox", True) # connect to inbox.
    return_code, mail_ids = mail.search(None, 'UnSeen')
    count = len(mail_ids[0].split(" "))
except:
    count = 0

print(count)
