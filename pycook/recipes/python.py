def http_server(recipe):
    return [
        "python -m SimpleHTTPServer &",
        "python_process=$!",
        "sleep 0.5 && xdg-open http://localhost:8000",
        "wait $python_process",
    ]
