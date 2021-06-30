UTILS_PATH := build-utils
TEMPLATES_PATH := .

SERVICE_NAME := swag-api-keys
BUILD_IMAGE_TAG := 07d3946f8f005782697de20270ac58cdcd18b011

CALL_ANYWHERE := all install validate
CALL_W_CONTAINER := $(CALL_ANYWHERE)

all: validate

-include $(UTILS_PATH)/make_lib/utils_container.mk

.PHONY: $(CALL_W_CONTAINER)

install:
	npm install

validate:
	npm run validate
