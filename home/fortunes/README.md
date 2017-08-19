## What is Fortune?

Fortune is a simple Unix program that displays a random message from a database of quotations.

```
$ fortune

"What we see is mainly what we look for."
  ~Unknown
```

This directory contains some fortune database files.


## Install

First install [fortune package](http://linux.die.net/man/6/fortune). If your computer has already installed it, skip this step.

```bash
# Debian/Ubuntu
$ sudo apt-get install fortune

# Mac
$ brew install fortune
```

Then install the database files.

```bash
# Debian/Ubuntu
cp humorix-misc /usr/share/games/fortunes/
cp humorix-stories /usr/share/games/fortunes/
cp joel-on-software /usr/share/games/fortunes/
cp liberty /usr/share/games/fortunes/
cp misc /usr/share/games/fortunes/
cp osp /usr/share/games/fortunes/
cp rkba /usr/share/games/fortunes/
cp tao /usr/share/games/fortunes/

# Mac
strfile humorix-misc
strfile humorix-stories
strfile joel-on-software
strfile liberty
strfile misc
strfile osp
strfile rkba
strfile tao
mv *.dat /usr/local/share/games/fortunes/
```

## Usage

```bash
$ fortune [OPTIONS] [/path/to/fortunes]
```

Options

```
- -c     Show the cookie file from which the fortune came.
- -f     Print out the list of files which would be searched, but don't print a fortune.
- -e     Consider all fortune files to be of equal size.
```

Example of `-c`

```bash
$ fortune -c

(fortunes)
%
"Don't waste life in doubts and fears."
  ~Ralph Waldo Emerson
```

## How to automatically launch fortune when opening a shell window

Depending on which shell you use, at the end of your `~/.bashrc` or `~/.zshrc` file, copy the following lines into it.

```bash
echo
echo "=============== Quote Of The Day ==============="
echo
fortune
echo
echo "================================================"
echo
```

## How to make your own fortune database file

1. Write your fortune items into a file.
2. Append a percent sign (%) after each item. The percent sign should take a new line. The following is an example.

    ```
    A day for firm decisions!!!!!  Or is it?
    %
    A few hours grace before the madness begins again.
    %
    A gift of a flower will soon be made to you.
    %
    A long-forgotten loved one will appear soon.

    Buy the negatives at any price.
    %
    A tall, dark stranger will have more fun than you.
    ```

3. Generate the index file.

    ```bash
    strfile -c % your-fortune-file your-fortune-file.dat
    ```

4. Move the fortune file and its index file into `/usr/share/games/fortunes/`.
