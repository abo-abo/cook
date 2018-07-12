def force_service_restart(recipe):
    """Useful when Docker stops working after upgrading kernel."""
    return [
        "sudo rm -rf /var/lib/docker",
        "sudo service docker restart"
    ]
