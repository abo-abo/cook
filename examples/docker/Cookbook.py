#* Recipes
def setup_install_docker(recipe):
    return [
        "sudo apt-get update",
        "sudo apt-get install linux-image-extra-$(uname -r) linux-image-extra-virtual",
        "sudo apt-get update",
        "curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -",
        "sudo apt-get install docker-ce",
        "docker --version"]

def setup_run_docker_without_sudo(recipe):
    return [
        "sudo groupadd docker || echo 'ignore'",
        "sudo gpasswd -a $USER docker",
        "echo 'Log out/in to make results premanent'"]

def docker_list_all_containers(recipe):
    return ["docker ps"]

def docker_list_all_images(recipe):
    return ["docker images"]

def docker_stop_all_containers(recipe):
    return ["docker stop $(docker ps -aq)"]

def docker_remove_stopped_containers(recipe):
    return ["docker rm $(docker ps -aq)"]

def docker_remove_untagged_images(recipe):
    return ["docker rmi $(docker images | grep '^<none>' | awk '{print $3}')"]
