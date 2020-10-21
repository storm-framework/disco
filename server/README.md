# server

## TODO 

- [*] compile 
- [*] run
- [*] add rjhala@eng.ucsd.edu as admin
- [*] UPDATE /signurl route handler
- [ ] WRITE  PUT /photo/:id handler
- [ ] WRITE  GET /photo/:id handler

1. Use `Binah.JSON.decodeFiles` to (PUT)
    - get the files + types
    - stash in the db as BS

2. Use `Binah.JSON.respondBlob` to
    - send the blob back to the user (GET)

## Dependencies

You need to have stack in your `$PATH` for the following to work. 

If you want to edit `Model.binah` you'll also need [binah-codegen](https://github.com/nilehmann/binah-codegen) 

See below for further instructions. 

## Build the code

```
$ make build
```

## Run the server

The following will start the server in 127.0.0.0:3000

```
$ stack run
```

## Database Migrations

Persistent is not very smart out of the box to figure out how to run migrations. If you ever find an
error related to migrations when running the server removing the database should fix it.

```bash
$ rm db.sqlite
```

## AWS Credentials

We are using S3 to upload photos. 

To upload photos you need to setup the following environment variables to match and account and bucket in S3.

```
DISCO_AWS_ACCESS_KEY
DISCO_AWS_SECRET_KEY
DISCO_AWS_REGION
DISCO_AWS_BUCKET
```

## SMTP

If you want to send emails you need to configure the following environment variables to match an
SMTP server. The connection will be over SSL.

```
DISCO_SMTP_HOST
DISCO_SMTP_PORT
DISCO_SMTP_USER
DISCO_SMTP_PASS
```

## Add organizer user

To add an organizer run

```
$ stack run -- add-organizer --email="email@domain.com" --password "password"
```

This will add an organizer without profile info. You can edit the profile in the app later.

## Note on editing `Model.binah`

If you edit `Model.binah` you'll first need to generate the corresponding `Model.hs`. 
The command `make build` is a wrapper over `stack build` that auto generate `Model.hs` 
from `Model.binah` when necessary. For this to work you need to have `binah-codegen` 
in your `$PATH`. See https://github.com/nilehmann/binah-codegen for instructions. 
If you don't modify `Model.binah` things should work just fine because `Model.hs` 
is under version control. Alternatively you could also run `make model` to generate 
`Model.hs`.
