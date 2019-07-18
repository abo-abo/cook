def http_server(recipe):
    return [
        "python -m SimpleHTTPServer &",
        "xdg-open http://localhost:8000"
    ]
