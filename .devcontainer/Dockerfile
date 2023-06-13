# ARG VARIANT=7.0-bullseye-slim
# Feeling adventurous https://hub.docker.com/_/microsoft-dotnet-nightly-sdk/
ARG VARIANT=8.0-preview-bookworm-slim
FROM mcr.microsoft.com/dotnet/nightly/sdk:${VARIANT}
ENV PATH $PATH:/home/vscode/.dotnet:/home/vscode/.dotnet/tools