def list_all_containers(recipe):
    return ["docker ps"]

def list_all_images(recipe):
    return ["docker images"]

def stop_all_containers(recipe):
    return ["docker stop $(docker ps -aq)"]

def remove_stopped_containers(recipe):
    return ["docker rm $(docker ps -aq)"]

def remove_untagged_images(recipe):
    return ["docker rmi $(docker images | grep '^<none>' | awk '{print $3}')"]

def force_service_restart(recipe):
    """Useful when Docker stops working after upgrading kernel."""
    return [
        "sudo rm -rf /var/lib/docker",
        "sudo service docker restart"
    ]

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
