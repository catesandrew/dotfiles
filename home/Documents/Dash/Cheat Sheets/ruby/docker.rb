cheatsheet do
  title 'Docker'
  docset_file_name 'Docker'
  keyword 'docker'
  source_url 'http://cheat.kapeli.com'

  category do
    id 'Aliases'

    entry do
      name 'Get latest container ID'
      command 'dl'
      notes "
        ```
        docker ps -l -q
        ```"
    end

    entry do
      name 'Get container process'
      command 'dps'
      notes "
        ```
        docker ps
        ```"
    end

    entry do
      name 'Get process included stop container'
      command 'dpa'
      notes "
        ```
        docker ps -a
        ```"
    end

    entry do
      name 'Get images'
      command 'di'
      notes "
        ```
        docker images
        ```"
    end

    entry do
      name 'Get container IP'
      command 'dip'
      notes "
        ```
        docker inspect --format \"{{ .NetworkSettings.IPAddress }}\"
        ```"
    end

    entry do
      name 'Run deamonized container, e.g., $dkd base /bin/echo hello'
      command 'dkd'
      notes "
      ```
      docker run -d -P
      ```"
    end

    entry do
      name 'Run interactive container, e.g., $dki base /bin/bash'
      command 'dki'
      notes "
        ```
        docker run -i -t -P
        ```"
    end

    entry do
      name 'Stop all containers'
      command 'dstop'
      notes "
        ```
        docker stop $(docker ps -a -q);
        ```"
    end

    entry do
      name 'Remove all containers'
      command 'drm'
      notes "
        ```
        docker rm $(docker ps -a -q);
        ```"
    end

    entry do
      name 'Stop and Remove all containers'
      command 'drmf'
      notes "
        ```
        docker stop $(docker ps -a -q) && docker rm $(docker ps -a -q)
        ```"
    end

    entry do
      name 'Remove all images'
      command 'dri'
      notes "
        ```
        docker rmi $(docker images -q);
        ```"
    end

    entry do
      name 'Dockerfile build, e.g., $dbu tcnksm/test'
      command 'dbu'
      notes "
        ```
        docker build -t=$1 .;
        ```"
    end

    entry do
      name 'Show all alias related docker'
      command 'dalias'
      notes "
        ```
        alias | grep 'docker' | sed \"s/^\([^=]*\)=\(.*\)/\1 => \2/\"| sed \"s/['|\']//g\" | sort;
        ```"
    end

    entry do
      name 'boot2docker ip'
      command 'docker-ip'
      notes "
        ```
        boot2docker ip 2> /dev/null
        ```"
    end

    entry do
      name 'enter into a running container'
      command 'dent'
      notes "
        ```
        docker exec -i -t $1 /bin/bash
        ```"
    end

    entry do
      name 'run bash for any image. dbash is particularly useful when diagnosing a failed `docker build`. Just dbash the last generated image and re-run the failed command.'
      command 'dbash '
      notes "
        ```
        docker run --rm -i -t -e TERM=xterm --entrypoint /bin/bash $1 
        ```"
    end

  end
end
