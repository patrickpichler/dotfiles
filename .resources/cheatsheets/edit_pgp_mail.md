* Change email
```
$ gpg --edit-key <keyID>
gpg> adduid
Real name: <name>
Email address: <email>
Comment: <comment>
Change (N)ame, (C)omment, (E)mail or (O)kay/(Q)uit? o
You need a passphrase to unlock the secret key for
user: "foo <foo@bar.com>"
```

* trust new UID
```
gpg> uid <new uid number>
gpg> trust
Your decision? 5
Do you really want to set this key to ultimate trust? (y/N) y
gpg> uid <new uid number>
```

* revoke old UID
```
gpg> uid <old uid number>
gpg> revuid
Really revoke this user ID? (y/N) y
Your decision? 4
Enter an optional description; end it with an empty line: <description>
Is this okay? (y/N) y
```

* save changes
```
gpg> save
$ gpg --keyserver hkp://pgp.mit.edu --send-keys <keyID>
```

(credit https://coderwall.com/p/tx_1-g/gpg-change-email-for-key-in-pgp-key-servers)
