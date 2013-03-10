# SimpleIRC
SimpleIRC is a simple IRC Library for haskell.

## Installation
To install SimpleIRC as a library you need cabal.

You can then use `cabal install` to install.

## Example
For an example on how to use the library see example/example.hs and tests/testbot.hs

I'm also creating an IRC Bot which uses this library.
[Take a look at it](http://github.com/dom96/ElysiaBot "Take a look at it") for more practical usage of the lib.

### Usage
The first step is to create an IrcConfig datatype. You have to specify the address of the server,
the server port, nickname, username, realname, list of channels to join when connected,
and a list of events which will be called when certain commands are received from the server.

You can specify 0 events, if for some unlikely reason you want your bot to not respond to anything.
But in the more likely event when you do want to specify functions, you can do that simply by creating a function with the type of _EventFunc_

    onMessage :: EventFunc

EventFunc has the type of `(IrcServer -> IrcMessage -> IO ())`.
For Haskell beginners that's a function which takes two arguments; an IrcServer and an IrcMessage, and which returns a IO ()

    onMessage server msg
      | m == "|hello" = do
        sendMsg s chan "hello!"
      | otherwise = return ()
      where chan = fromJust $ mChan m
            m    = mMsg msg

This function will send "hello" to a channel whenever someone says "|hello".

Then you can pass `[(Privmsg onMessage)]` to IrcConfig.

Take a look at Network/SimpleIRC/Types.hs to see other events.

There is one(as of now) event which doesn't take a EventFunc function.
You should be able to figure out what type of function it does take by looking at Network/SimpleIRC/Types.hs

After you create a IrcConfig you can then call `connect`. Connect takes two arguments; the IrcConfig and a boolean.
The boolean specifies whether to run the listenLoop in a new thread.

If you have any questions you can ask them @ irc://irc.freenode.net/#()

## License
SimpleIRC is licensed under the BSD3 license. Read the license file for more information.

