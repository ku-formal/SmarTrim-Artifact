FROM python:3.11-slim
RUN pip install --upgrade pip && python3 -m pip install slither-analyzer

CMD ["/bin/bash"]