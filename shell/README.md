# Shell

## ShellCheck

[ShellCheck][] can catch many bug risks in shell scripts: we encourage running
it against all scripts. There is also a Code Climate engine available wrapping
ShellCheck: we recommend enabling this engine on repos that contain significant
shell scripts.

[ShellCheck]: https://github.com/koalaman/shellcheck

## Compatability

`/bin/sh` should be preferred to `/bin/bash` for compatability unless Bash-only
features are really needed. Be aware that many modern systems actually alias
`/bin/bash` to `/bin/sh`, so if you use Bash-isms accidentally you may not
realize it. If you want to be sure your shell is `sh`-compliant, you may want to
install the [`dash`] shell & use that to test your scripts.

By the same token, using POSIX flags & functionality that will work between most
flavors of \*nix is preferred when possible.

[dash]: https://en.wikipedia.org/wiki/Almquist_shell#dash:_Ubuntu.2C_Debian_and_POSIX_compliance_of_Linux_distributions

## Error reporting

All scripts should call `set -e` to ensure the script will exit immediately if
any intermediate command errors.

## Variable and function names

Names of variables and functions should be snake\_cased. Variable names should
not be UPPER\_CASED unless being exported to the `ENV`.

## Exit codes

The general principle, of course, is that `0` indicates success and `1`
indicates a general error. More complex scripts may want to define specific
codes for different kinds of errors: please refer to the [Bash Scripting
Guide][bsg_exitcodes] & [`sysexits`][man_sysexits] for reserved/defined codes.
One common one we use in many scripts is `64` to indicate incorrect arguments
passed to a script.

[bsg_exitcodes]: http://www.tldp.org/LDP/abs/html/exitcodes.html
[man_sysexits]: http://www.gsp.com/cgi-bin/man.cgi?topic=sysexits

## Calling binaries

Long-form flags (e.g. `ls --all` instead of `ls -a`) are preferred: code is read
more often than it's written.  Long flags help keep scripts easy to understand &
maintain, and written scripts don't have the same need for brevity that typing
at an interactive terminal does.

## String interpolation for output

Interpolating variables within strings passed to `echo` can have unexpected
results: `printf` is preferred.

```shell
# Good

printf "Hello %s\n" "$name"

# Bad

echo "Hello $name"

# This is fine: no variable involved

echo "Hello world."
```

## Resources

* [Rich's sh Tricks](http://www.etalabs.net/sh_tricks.html)
* [IEEE Std 1003.1-2001](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_05_03)
* [Advanced Bash Scripting Guide](http://www.tldp.org/LDP/abs/html/index.html):
  a very in-depth guide to a wide range of shell scripting needs, though be
  aware that it is Bash-focused & doesn't clearly say what's Bash-specific and
  what's not. I wish there were a guide as good as this for POSIX `sh`.
