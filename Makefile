SHELL := /bin/bash
PATH := $(CURDIR)/.cargo/bin:$(PATH)

all: $(CURDIR)/.cargo/bin/cargo
	CARGO_HOME=$(CURDIR)/.cargo RUSTUP_HOME=$(CURDIR)/.rustup cargo build --release

$(CURDIR)/.cargo/bin/cargo:
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | CARGO_HOME=$(CURDIR)/.cargo RUSTUP_HOME=$(CURDIR)/.rustup sh -s -- -y --no-modify-path

clean:
	if command -v cargo; then CARGO_HOME=$(CURDIR)/.cargo RUSTUP_HOME=$(CURDIR)/.rustup cargo clean; fi;
	cd $(CURDIR) && rm -rf .rustup .cargo
