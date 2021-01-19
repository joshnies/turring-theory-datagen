from theory_data_gen.common import gen_item, gen_mask_token
from theory_data_gen.constants import MEMBER_TOKEN
from theory_data_gen.generator import Generator


class JderegJavaUtilToNodeJs14Generator(Generator):
    """
    Case generator for "jdereg/java-util":
    https://github.com/jdereg/java-util

    Target: Node.js 14
    """

    @staticmethod
    def generate(args, write):
        print('\nGenerating case data for "jdereg/java-util" --> Node.js 14')

        items = [
            gen_item(
                f'public class {gen_mask_token(0)}<{gen_mask_token(1)}, {gen_mask_token(2)}> implements Map<{gen_mask_token(3)}, {gen_mask_token(4)}> {{'
                f'class {gen_mask_token(0)} implements Map {{'
            ),
            gen_item(
                f'{MEMBER_TOKEN} private final Map<{gen_mask_token(0)}, {gen_mask_token(1)}> {gen_mask_token(2)};'
                f'private {gen_mask_token(2)};'
            ),
            gen_item(
                f'{MEMBER_TOKEN} private final Set<{gen_mask_token(0)}> {gen_mask_token(1)};'
                f'private {gen_mask_token(1)};'
            ),
            gen_item(
                f'public {gen_mask_token(0)}(Map<{gen_mask_token(1)}, {gen_mask_token(2)}> {gen_mask_token(3)}) {{'
                f'constructor({gen_mask_token(3)}) {{'
            ),
            gen_item(
                f'if ({gen_mask_token(0)} == null) {{'
            ),
            gen_item(
                f'throw new IllegalArgumentException({gen_mask_token(0)});'
                f'throw new Error({gen_mask_token(0)});'
            ),
            gen_item(
                f'{gen_mask_token(0)} = {gen_mask_token(1)};'
            ),
            gen_item(
                f'{gen_mask_token(0)} = new HashSet<>();',
                f'{gen_mask_token(0)} = new Set();'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public {gen_mask_token(0)} {gen_mask_token(1)}(Object {gen_mask_token(2)}) {{',
                f'{gen_mask_token(1)}(key) {{'
            ),
            gen_item(
                f'{gen_mask_token(0)} {gen_mask_token(1)} = {gen_mask_token(2)}.{gen_mask_token(3)}({gen_mask_token(4)});',
                f'let {gen_mask_token(1)} = {gen_mask_token(2)}.{gen_mask_token(3)}({gen_mask_token(4)});'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}(({gen_mask_token(2)}) {gen_mask_token(3)});',
                f'{gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(3)});'
            ),
            gen_item(
                f'return {gen_mask_token(0)};'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public {gen_mask_token(0)} {gen_mask_token(1)}({gen_mask_token(2)} {gen_mask_token(3)}, {gen_mask_token(4)} {gen_mask_token(5)}) {{',
                f'{gen_mask_token(1)}({gen_mask_token(3)}, {gen_mask_token(5)}) {{'
            ),
            gen_item(
                f'return {gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(2)}, {gen_mask_token(3)});'
            ),
            gen_item(
                f'return {gen_mask_token(0)}({gen_mask_token(1)}, {gen_mask_token(2)});',
            ),
            gen_item(
                f'{MEMBER_TOKEN} public boolean {gen_mask_token(0)}({gen_mask_token(1)} {gen_mask_token(2)}) {{',
                f'{gen_mask_token(0)}({gen_mask_token(2)}) {{'
            ),
            gen_item(
                f'boolean {gen_mask_token(0)} = {gen_mask_token(1)}.{gen_mask_token(2)}({gen_mask_token(3)});'
                f'let {gen_mask_token(0)} = {gen_mask_token(1)}.{gen_mask_token(2)}({gen_mask_token(3)});'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public void {gen_mask_token(0)}(Map<? extends {gen_mask_token(1)}, ? extends {gen_mask_token(2)}> {gen_mask_token(3)}) {{',
                f'{gen_mask_token(0)}({gen_mask_token(3)}) {{'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(2)});'
            ),
            gen_item(
                f'return {gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(2)});'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public int {gen_mask_token(0)}() {{',
                f'{gen_mask_token(0)}() {{'
            ),
            gen_item(
                f'return {gen_mask_token(0)}.{gen_mask_token(1)}();'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public boolean {gen_mask_token(0)}() {{',
                f'{gen_mask_token(0)}() {{'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public boolean {gen_mask_token(0)}(Object {gen_mask_token(1)}) {{',
                f'{gen_mask_token(0)}({gen_mask_token(1)}) {{'
            ),
            gen_item(
                f'return {gen_mask_token(0)} instanceof Map && {gen_mask_token(1)}.{gen_mask_token(2)}({gen_mask_token(3)});',
                f'return {gen_mask_token(0)} instanceof Object && {gen_mask_token(1)}.{gen_mask_token(2)}({gen_mask_token(3)});'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public String {gen_mask_token(0)}() {{',
                f'{gen_mask_token(0)}() {{'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public void {gen_mask_token(0)}() {{',
                f'{gen_mask_token(0)}() {{'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}();',
            ),
            gen_item(
                f'{MEMBER_TOKEN} public Collection<{gen_mask_token(0)}> {gen_mask_token(1)}() {{',
                f'{gen_mask_token(1)}() {{'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public Set<{gen_mask_token(0)}> {gen_mask_token(1)}() {{',
                f'{gen_mask_token(1)}() {{'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public Set<Entry<{gen_mask_token(0)}, {gen_mask_token(1)}>> {gen_mask_token(2)}() {{',
                f'{gen_mask_token(2)}() {{'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}().{gen_mask_token(2)}({gen_mask_token(3)});'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public void {gen_mask_token(0)}(Collection<{gen_mask_token(1)}> {gen_mask_token(2)}) {{',
                f'{gen_mask_token(0)}({gen_mask_token(2)}) {{'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public void {gen_mask_token(0)}({gen_mask_token(1)}<{gen_mask_token(2)}, {gen_mask_token(3)}> {gen_mask_token(4)}) {{',
                f'{gen_mask_token(0)}({gen_mask_token(4)}) {{'
            ),
            gen_item(
                f'{gen_mask_token(0)}.{gen_mask_token(1)}({gen_mask_token(2)}.{gen_mask_token(3)});'
            ),
            gen_item(
                f'{MEMBER_TOKEN} public Map {gen_mask_token(0)}() {{',
                f'{gen_mask_token(0)}() {{'
            ),
            gen_item(
                f'return {gen_mask_token(0)}();'
            ),
            gen_item(
                f'{gen_mask_token(0)}(({gen_mask_token(1)}){gen_mask_token(2)});',
                f'{gen_mask_token(0)}({gen_mask_token(2)});'
            ),
            gen_item(
                f'{gen_mask_token(0)}({gen_mask_token(1)});'
            ),
            gen_item(
                f'return {gen_mask_token(0)}({gen_mask_token(1)});'
            ),
            gen_item(
                f'return {gen_mask_token(0)}();'
            ),
            gen_item(
                f'{gen_mask_token(0)}();'
            )
        ]

        for i in items:
            write(i)
