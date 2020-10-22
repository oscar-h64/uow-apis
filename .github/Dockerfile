# this Dockerfile assumes that a binary has already been built
# that is compatible with ubuntu:latest - since our GitHub
# workflow runs on the same platform and we already perform the
# build once, this saves us from performing the build a second time
FROM ubuntu:latest
WORKDIR /
COPY ./bin /
RUN chmod +x uow-util
ENTRYPOINT ["/uow-util"]