provider "aws" {
  region = "us-west-2"
}

resource "aws_instance" "ci_build" {
  ami = "ami-28e07e50" # RHEL 7.5
  instance_type = "t3.large"
  key_name = "hive-ci"
  security_groups = [
    "${aws_security_group.allow_all.name}",
  ]

  root_block_device {
    volume_size = 25
  }

  tags {
    Name = "Hive CI Build"
  }
}

resource "aws_security_group" "allow_all" {
  name_prefix = "allow-all-"
  ingress {
    from_port = 0
    to_port = 0
    protocol = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    from_port = 0
    to_port = 0
    protocol = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

output "ip" {
  value = "${aws_instance.ci_build.public_ip}"
}

output "id" {
  value = "${aws_instance.ci_build.id}"
}
