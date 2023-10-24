use russx::Template;

russx::templates! {
    /// info
    pub fn greet<'a>(
        #[prop(default = "Jeff")]
        name: &'a str,
        #[prop(toggle)]
        do_it: bool,
    ) {
        <script>
            "let a = 4;"
        </script>
        "\n"
        <p>
            <if {do_it}>
                Hello {name}
            <else if let(len @ ..=3) {name.len()}>
                Hello tiny {name} of {len}
            <else>
                Welcome
            </if>

            <label>
                "Change your name here:"
                <input type="search" name="q" value=name>
                <trust {"<p>Hello</p>"}>
            </label>
        </p>

        <match {100}>
        <on case(i @ 0..=100) if {i != 55}>
            <p>Hello {i}</p>
        <on case(i)>
            <p>Hello {i}</p>
        </match>
    }

    pub fn greet2<'a>(
        names: &'a [&'a str],
    ) {
        <h1>Greet 2 people</h1>
        <for each(name) in {names}>
            <greet name=name do_it />
        </for>
    }

    pub fn base<'a>(
        #[prop(default)]
        title: &'a str,
        #[prop(into, default)]
        head: russx::TemplateFn<'a>,
        #[prop(into, default)]
        children: russx::TemplateFn<'a>,
    ) {
        <!DOCTYPE html>
        <html>
            <head>
                <title>{title}</title>
                <{head} />
            </head>
            <body>
                <{children} />
            </body>
        </html>
    }

    pub fn index<'a>(name: &'a str) {
        <self::base title="hello">
            <prop head>
                <greet name="foo" />
            </prop>
            hello {name}
        </self::base>
    }
}

fn main() {
    let html = greet2(&["George", "Michael"]).render().unwrap();
    println!("{html}");
}