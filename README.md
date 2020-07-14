# Distant Socialing

The app is divided in a [client](https://github.com/nilehmann/binah-apps/tree/master/covid/client) written with [vue.js](https://vuejs.org/) and a [server](https://github.com/nilehmann/binah-apps/tree/master/covid/server) written with Binah.

## TODO

- [Fancy Modal Message OK](https://bootstrap-vue.org/docs/components/modal#confirm-message-box)
- [Modal Form](https://stackoverflow.com/questions/53269544/how-to-submit-form-data-from-b-modal-in-vue)

Issues

1. Models
2. Return-in-modal
3. Message-to-user
4. Reply-to-sender

DB Models

```haskell

-- | DB Models

Message
  sender    UserId
  receiver  UserId Maybe
  message   Text
  timestamp Int64

MarkRead
  user      UserId
  message   MessageId 
```

-- | POST /api/message/send

sendMessage msg = 
  create-and-insert relevant record

-- | POST /api/message/read/:msgId

markRead msgId = do
  userId <- requireAuthUser
  readMb <- selectFirst (markReadUser ==. userId)
  case readMb of
    Nothing   -> insert-NEW-RECORD
    Just read -> insert-UPDATED-RECORD with max of read.message

-- | GET /api/message/receive

getMessages = do
  userId   <- requireAuthUser
  readUpto <- getReadUpto userId
  public   <- selectList [ readUpto <=. messageId, messageReceiver ==. Nothing, ]
  private  <- selectList [ readUpto <=. messageId, messageReceiver ==. Just userId ]
  return (public ++ private)
```

## Development

### Get the code

Clone the repo including submodules

```bash
$ git clone --recursive https://github.com/nilehmann/binah-apps/
```

### Run the code

For local development you need to run both the [client](https://github.com/nilehmann/binah-apps/tree/master/covid/client#readme) and [server](https://github.com/nilehmann/binah-apps/tree/master/covid/server#readme). Se each README for further instruction.

## Production

### Build the code

To build the code for production run:

```bash
$ ./build.sh
```

This will create bundle files for the client app and compile the haskell code. It'll then copy everything to `dist/`.

Alternatively, you can specify a docker image to build the binaries. Only the haskell code will be built inside the container. The image needs to have stack installed.

```bash
$ ./build.sh -i image
```

### Run the code

In production you need to run the server with `--static` specifying a directory to serve static files from. When `--static` is specified the server will also respond for any unknown route with `index.html` (routes are handled in the client by vue).
For example, assuming you built the code with `build.sh` you can run the server like this:

```bash
$ dist/disco --static=dist/static
```


### TODO

**Feature: Alert**

1. console.log every heartbeat
2. alert on every 10 heartbeats using bvModal

