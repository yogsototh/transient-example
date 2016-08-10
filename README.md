# Stack Transient Repository

This is a stack project which implement this example Transient project:

[see this thread on Reddit](https://www.reddit.com/r/elm/comments/4wq3ko/playing_with_websockets_in_haskell_and_elm/d69o11p)

## Usage

1. Install [`stack`](haskellstack.org)
2. Install ghc and ghcjs with stack.
   If you never installed ghcjs that could take a _lot_ of time.
   
   ~~~
   stack setup
   stack --stack-yaml stack-ghcjs.yaml setup
   ~~~

3. Compile the example
   
   ~~~
   stack build
   stack --stack-yaml stack-ghcjs.yaml build
   ~~~

4. Link the result of the GHCJS compilation in the right directory
   
   ~~~
   ./mklink.sh
   ~~~

5. Start the server
   
   ~~~
   ./start.sh
   ~~~

6. Open <http://localhost:3000> in many different web browsers and play.
