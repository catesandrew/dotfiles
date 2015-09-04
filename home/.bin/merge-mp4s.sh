#!/bin/bash
shopt -s nullglob

function walk_tree {
      # echo "Directory: $1"
      local directory="$1"
      local final
      local i
      for i in "$directory"/*;
      do
      # echo "File: $i"
      if [ "$i" = . -o "$i" = .. ]; then
            continue
        elif [ -d "$i" ]; then  # Process directory and / or walk-down into directory
            echo "Directory: $i"
            final=""
            cd "$i"
            for l in *.mp4;
            do
                final+=" -cat \"$l\"";
            done;

            echo $final
            if [ -n "$final" ]; then
                cmd="MP4Box $final -new \"../${PWD##*/}.mp4\""
                eval "$cmd"
            fi


            final=""
            for l in *.m4v;
            do
                final+=" -cat \"$l\"";
            done;

            if [ -n "$final" ]; then
                cmd="MP4Box $final -new \"../${PWD##*/}.m4v\""
                eval "$cmd"
            fi

            cd ..

            # add command here to process all files in directory (i.e. ls -l "$i/"*)
            walk_tree "$i"      # DO NOT COMMENT OUT THIS LINE!!
        else
            continue    # replace continue to process individual file (i.e. echo "$i")
        fi
      done
}

walk_tree $1

# final="";for i in *.mp4; do final+=" -cat \"$i\""; done; echo "MP4Box $final -new \"../${PWD##*/}.mp4\"" | pbcopy

# MP4Box  -cat "*.mp4" -new "../BIWS Premium - Financial Modeling Fundamentals.mp4"

# MP4Box  -cat "01-06-Interview-Model-Overview.mp4" -cat "01-07-Walk-Through-Statements.mp4" -cat "01-08-Depreciation-Changes.mp4" -cat "01-09-Inventory-Changes-Cash-Debt.mp4" -cat "01-10-Inventory-LIFO-vs-FIFO.mp4" -cat "01-11-Accrued-Expenses.mp4" -cat "01-12-Deferred-Revenue.mp4" -cat "01-13-Deferred-Income-Taxes.mp4" -cat "01-14-Dividends.mp4" -cat "01-15-Issue-Repurchase-Shares.mp4" -cat "01-16-Raising-Paying-Off-Debt.mp4" -cat "01-17-Bailout.mp4" -cat "01-18-Goodwill-Impairment.mp4" -cat "01-19-Writing-Down-Debt.mp4" -cat "01-20-iPhone-Factories-Debt-Part-1.mp4" -cat "01-21-iPhone-Factories-Debt-Part-2.mp4" -cat "01-22-iPhone-Factories-Debt-Part-3.mp4" -cat "01-23-Making-iPhones-Part-1.mp4" -cat "01-24-Making-iPhones-Part-2.mp4" -cat "10-01-Income-Statement.mp4" -cat "10-02-Balance-Sheet-Assets.mp4" -cat "10-03-Balance-Sheet-Liabilities-SE.mp4" -cat "10-04-Cash-Flow-Statement.mp4" -cat "10-05-Linking-Statements.mp4" -new "../01 Accounting Fundamentals.mp4"


