## Requirements

### Operating System

We recommend Ubuntu 22.04 or 24.04. (We tested on 22.04)

### Hardware

To ensure stable reproduction of the experimental results, we recommend a computer with at least 64 threads and 62 GB of RAM.

### Software

* `docker` ([install](https://docs.docker.com/engine/install/ubuntu/))

We assume `docker` is installed in your hardware, as we will be reproducing the experiment in a Dockerized environment.

To run `docker` without `sudo`, execute `sudo usermod -aG docker $USER`. (reference: https://askubuntu.com/a/739861)
